----------------------------------------------------------------
--
-- Informl
-- Meta-language for quickly defining DSLs, including their
-- abstract syntax data structures, parsers, prettyprinters,
-- and other transformations.
--
-- Language/Informl/Compilation.hs
--   Haskell implementation of the Informl language compilation
--   monad.
--

----------------------------------------------------------------
-- Haskell implementation of the Informl language compilation
-- monad.

module Language.Informl.Compilation
  where

import Language.Informl.AbstractSyntax

----------------------------------------------------------------
-- Data types and class memberships.

type FreshIndex = Integer
type Indentation = String
type ModuleName = String
type Qualifier = String
type Constructor = String
type QualEnv = [(Qualifier,[(Constructor,Int)])] -- user defined qualifiers
type DefEnv = [(Definer,String,Bool)] -- user defined characteristics for ADTs
type NestingDepth = Integer
type Raw = String
type NameSpace = [(String,[String])]

data State = 
  State FreshIndex Indentation (Maybe ModuleName) QualEnv DefEnv NameSpace NestingDepth Raw

empState = State 0 "" Nothing [] [] [("Informl", stdlibFuncs)] 0 ""

data Compilation a = Compilation (State -> (State, a))

-- Standard state monad definition.
instance Monad Compilation where
  return x = Compilation (\s -> (s, x))
  (>>=) (Compilation c1) fc2 = Compilation $
    (\state ->
      let (state', r) = c1 state
          Compilation c2 = fc2 r
      in c2 state'
    )

----------------------------------------------------------------
-- Combinators and functions.

stdlibFuncs = ["range", "join", "type"]

extract :: Compilation () -> String
extract (Compilation c) = let (State _ _ _ _ _ _ _ s, _) = c empState in s

freshWithPrefix :: String -> Compilation String
freshWithPrefix p = Compilation $ \(State f i m q d ns n s) -> (State (f+1) i m q d ns n s, p ++ show f)

setModule :: String -> Compilation ()
setModule m = Compilation $ \(State f i _ q d ns n s) -> (State f i (Just m) q d ns n s, ())

setQualEnv :: [StmtLine] -> Compilation ([(Constructor,Int)])
setQualEnv ss = let (qual,unqual) = getQualified ss in
         Compilation $ \(State f i m _ d ns n s) -> (State f i m qual d ns n s, (unqual))

setDefEnv :: DefEnv -> Compilation ()
setDefEnv d = Compilation $ \(State f i m q _ ns n s) -> (State f i m q d ns n s, ())

getModule :: Compilation (Maybe String)
getModule = Compilation $ \(State f i m q d ns n s) -> (State f i m q d ns n s, m)

getQualEnv :: Compilation (QualEnv)
getQualEnv = Compilation $ \(State f i m q d ns n s) -> (State f i m q d ns n s, q)

getDefEnv :: Compilation (DefEnv)
getDefEnv = Compilation $ \(State f i m q d ns n s) -> (State f i m q d ns n s, d)

expandNameSpace :: NameSpace -> Compilation()
expandNameSpace names = Compilation $ \(State f i m q d ns n s) -> (State f i m q d (names ++ ns) n s, ())

getNameSpace :: Compilation (NameSpace)
getNameSpace = Compilation $ \(State f i m q d ns n s) -> (State f i m q d ns n s, ns)

nothing :: Compilation ()
nothing = Compilation $ \s -> (s, ())

indent :: Compilation ()
indent = Compilation $ \(State f i m q d ns n s) -> (State f ("  " ++ i) m q d ns n s, ())

unindent :: Compilation ()
unindent = Compilation $ \(State f i m q d ns n s) -> (State f (drop (min (length i) 2) i) m q d ns n s, ())

newline :: Compilation ()
newline = Compilation $ \(State f i m q d ns n s) -> (State f i m q d ns n (s ++ "\n" ++ i), ())

string :: String -> Compilation ()
string s' = Compilation $ \(State f i m q d ns n s) -> (State f i m q d ns n (s ++ s'), ())

raw :: String -> Compilation ()
raw = string

nest :: Compilation ()
nest = Compilation $ \(State f i m q d ns n s) -> (State f i m q d ns (n+1) s, ())

unnest :: Compilation ()
unnest = Compilation $ \(State f i m q d ns n s) -> (State f i m q d ns (n-1) s, ())

depth :: Compilation Integer
depth = Compilation $ \(State f i m q d ns n s) -> (State f i m q d ns n s, n)

maybeQualified :: Constructor -> Compilation (Maybe Qualifier)
maybeQualified c  = Compilation $ \(State f i m q d ns n s) -> (State f i m q d ns n s, (maybeQualifiedAux c q))

getQualified :: [StmtLine] -> (QualEnv, [(Constructor,Int)])
getQualified ss = qualifiedAs (predefined ss) (uniqueAllPatterns ss)

getQualifiedNow :: [StmtLine] -> QualEnv
getQualifiedNow ss = fst $ qualifiedAs (predefinedNow ss) (uniqueAllPatterns ss)

inStdlib :: String -> Bool
inStdlib f = elem f stdlibFuncs

maybeQualifiedAux :: Constructor -> QualEnv -> (Maybe Qualifier)
maybeQualifiedAux c ((ql,cs):qls) = if has cs c then Just ql else maybeQualifiedAux c qls
maybeQualifiedAux c [] = Nothing

has :: (Eq a) => [(a,b)] -> a -> Bool
has [] a = False
has ((x,y):xs) a 
    | a == x = True
    | otherwise = has xs a

gives :: (Eq a) => [(a,b)] -> a -> Maybe (a,b)
gives [] a = Nothing
gives ((x,y):xs) a 
    | a == x = Just (x,y)
    | otherwise = gives xs a

extractTup :: (Eq a) => a -> [(a,Int)] -> ((a,Int),[(a,Int)])
extractTup a [] = ((a,0),[])
extractTup a ((x,i):xs) = let (y,ys) = extractTup a xs in
                  if a == x then ((x,i),ys)
                  else (y,((x,i):ys))

extractTupGen :: (Eq a) => a -> [(a,[b])] -> ((a,[b]),[(a,[b])])
extractTupGen a [] = ((a,[]),[])
extractTupGen a ((x,i):xs) = let (y,ys) = extractTupGen a xs in
                  if a == x then ((x,i),ys)
                  else (y,((x,i):ys))

qualextract :: String -> [(Qualifier,[(String,Int)])] -> ((String,Int),[(Qualifier,[(String,Int)])])
qualextract s ((q,ts):qs) = let (ex,rest) = extractTup s ts  
                                (r,rs) = qualextract s qs in
                                if has ts s then (ex,((q,rest):rs))  
                                else (r,(q,ts):rs)
qualextract s [] = (("",0),[])

unique :: (Eq a) => [(a,b)] -> [(a,b)]
unique [] = []
unique ((a,b):xs)
    | has xs a = unique xs
    | otherwise = (a,b): (unique xs)

uniqueAllPatterns :: [StmtLine] -> [(Constructor, Int)]
uniqueAllPatterns s = unique $ allPatterns s

allPatterns :: [StmtLine] -> [(Constructor, Int)]
allPatterns [] = []
allPatterns ((StmtLine s):ss) = allPatternsStmt s ++ allPatterns ss

allPatternsBlock :: Block -> [(Constructor, Int)]
allPatternsBlock (Stmt s) = allPatternsStmt s
allPatternsBlock (Block ss) = allPatterns ss 

allPatternsStmt :: Stmt -> [(Constructor, Int)]
allPatternsStmt (Function _ _ b) = allPatternsBlock b
allPatternsStmt (Set _ e wb) = allPatternsExp e ++ foldr (++) [] (map allPatternsWhen wb)
allPatternsStmt (Get e wb) = allPatternsExp e ++ foldr (++) [] (map allPatternsWhen wb)
allPatternsStmt (For e b) = allPatternsExp e ++ allPatternsBlock b
allPatternsStmt (If e b) = allPatternsExp e ++ allPatternsBlock b
allPatternsStmt (ElseIf e b) = allPatternsExp e ++ allPatternsBlock b
allPatternsStmt (Else b) = allPatternsBlock b
allPatternsStmt (Global e) = allPatternsExp e
allPatternsStmt (Local e) = allPatternsExp e
allPatternsStmt (Return e) = allPatternsExp e
allPatternsStmt (StmtExp e) = allPatternsExp e
allPatternsStmt _ = [] 

allPatternsExp :: Exp -> [(Constructor, Int)] 
allPatternsExp (ConApp s es) = [(s,length es)] ++ foldr (++) [] (map allPatternsExp es)
allPatternsExp (Is e p) = allPatternsExp e ++ allPatternsPats p
allPatternsExp (IfExp e f g) = allPatternsExp e ++ allPatternsExp f ++ allPatternsExp g
allPatternsExp (Comma e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Ldots e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (And e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Or e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (In e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Subset e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Assign e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (PlusAssign e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (MinusAssign e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (MultAssign e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (DivAssign e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Eq e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Neq e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Lt e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Gt e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Leq e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Geq e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Union e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Intersect e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Max e) = allPatternsExp e
allPatternsExp (Min e) = allPatternsExp e
allPatternsExp (Domain e) = allPatternsExp e
allPatternsExp (Codomain e) = allPatternsExp e
allPatternsExp (Concat e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Plus e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Minus e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Mult e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Div e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Pow e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Not e) = allPatternsExp e
allPatternsExp (Neg e) = allPatternsExp e
allPatternsExp (Dot _ f) = allPatternsExp f -- skips the module reference
allPatternsExp (Parens e) = allPatternsExp e
allPatternsExp (Bracks e) = allPatternsExp e
allPatternsExp (Braces e) = allPatternsExp e
allPatternsExp (Bars e) = allPatternsExp e
allPatternsExp (FunApp v es) = foldr (++) [] (map allPatternsExp es)
allPatternsExp (Tuple es) = foldr (++) [] (map allPatternsExp es)
allPatternsExp (ListItem e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (DictItem k v) = allPatternsExp k ++ allPatternsExp v
allPatternsExp (Maps e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (Folds e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (On e f) = allPatternsExp e ++ allPatternsExp f
allPatternsExp (IndexItem e es) = allPatternsExp e ++ foldr (++) [] (map allPatternsExp es)
allPatternsExp _ = []

allPatternsWhen :: WhenBlock -> [(Constructor, Int)]
allPatternsWhen (WhenBlock p ss) = allPatternsPats p ++ allPatterns ss
allPatternsWhen (Otherwise ss) = allPatterns ss

allPatternsPats :: Pattern -> [(Constructor, Int)]
allPatternsPats (PatternCon s vs) = [(s, length vs)] ++  foldr (++) [] (map allPatternsPats vs)
allPatternsPats (AnonPattern vs) = foldr (++) [] (map allPatternsPats vs)
allPatternsPats _ = []

predefined :: [StmtLine] -> DefEnv
predefined ((StmtLine (Where ds)):ss) = (predefinedBuild $ map (\(DefLine d) -> d) ds) ++ (predefined ss)
predefined (s:ss) = predefined ss
predefined [] = []

predefinedNow :: [StmtLine] -> DefEnv
predefinedNow ((StmtLine (Where ds)):ss) = (predefinedBuildNow $ map (\(DefLine d) -> d) ds) ++ (predefinedNow ss)
predefinedNow (s:ss) = predefinedNow ss
predefinedNow [] = []

predefinedBuild :: [Definition] -> DefEnv
predefinedBuild []                   = []
predefinedBuild ((DefIs c d):ds)     = (predefinedBuild ds) ++ [(d,c,True)]
predefinedBuild ((DefAre cs d):ds)   = (predefinedBuild ds) ++ (map (\c -> (d,c,True)) cs)
predefinedBuild ((DefIsnt c d):ds)   = (predefinedBuild ds) ++ [(d,c,False)]
predefinedBuild ((DefArent cs d):ds) = (predefinedBuild ds) ++ (map (\c -> (d,c,False)) cs) 
predefinedBuild ((All d):ds)         = (predefinedBuild ds) ++ [(d,"all", True)]
predefinedBuild ((None d):ds)        = (predefinedBuild ds) ++ [(d,"none", True)]
predefinedBuild ((DefWas c d):ds)    = (predefinedBuild ds) ++ [(d,c,True)]
predefinedBuild ((DefWere cs d):ds)  = (predefinedBuild ds) ++ (map (\c -> (d,c,True)) cs)
predefinedBuild ((AllB4 d):ds)       = (predefinedBuild ds) ++ [(d,"all", True)]
predefinedBuild ((Import _ _):ds)    = (predefinedBuild ds)

predefinedBuildNow :: [Definition] -> DefEnv
predefinedBuildNow []                   = []
predefinedBuildNow ((DefIs c d):ds)     = (predefinedBuildNow ds) ++ [(d,c,True)]
predefinedBuildNow ((DefAre cs d):ds)   = (predefinedBuildNow ds) ++ (map (\c -> (d,c,True)) cs)
predefinedBuildNow ((DefIsnt c d):ds)   = (predefinedBuildNow ds) ++ [(d,c,False)]
predefinedBuildNow ((DefArent cs d):ds) = (predefinedBuildNow ds) ++ (map (\c -> (d,c,False)) cs) 
predefinedBuildNow ((All d):ds)         = (predefinedBuildNow ds) ++ [(d,"all", True)]
predefinedBuildNow ((None d):ds)        = (predefinedBuildNow ds) ++ [(d,"none", True)]
predefinedBuildNow (d:ds)               = (predefinedBuildNow ds)  

qualifiedAs :: DefEnv -> [(Constructor,Int)] -> (QualEnv, [(Constructor,Int)])
-- returns the first as the qualified data types and the second as unqualified
qualifiedAs (((Qualified v), s, b):ds) ps 
  | s == "all" && b =  ([(v,ps)],[])
  | b               = let (x,y) = qualifiedAs ds ps 
                          qd = gives x v 
                          (ex,rest) = extractTup s y
                          xNew = snd(extractTupGen v x) in
                          maybe (((v,[ex]):x),rest) (\(f,g)->(((f,ex:g):xNew),rest)) qd
  | otherwise       = qualifiedAs ds ps
qualifiedAs (((Unqualified), s, b):ds) ps
  | s == "all" && b = ([],ps)
  | b               = let (x,y) = qualifiedAs ds ps
                          ((ex,i),rest) = qualextract s x in
                          if ex == "" then (x,y)
                          else (rest,(ex,i):y)
  | otherwise       = qualifiedAs ds ps
qualifiedAs (d:ds) ps = qualifiedAs ds ps
qualifiedAs [] ps = ([],ps)

hasOtherwise :: [WhenBlock] -> [StmtLine]
hasOtherwise [] = []
hasOtherwise [Otherwise ss] = ss
hasOtherwise (w:ws) = hasOtherwise ws

compileLiteral :: [Char] -> String
compileLiteral [] = ""
compileLiteral ('\n':cs) = "\\n" ++ compileLiteral cs
compileLiteral ('\t':cs) = "  " ++ compileLiteral cs
compileLiteral ('\\':cs) = "\\\\" ++ compileLiteral cs
compileLiteral ('\"':cs) = "\\\"" ++ compileLiteral cs
compileLiteral (c:cs)    = c:(compileLiteral cs)

functionDecs :: [StmtLine] -> [String]
functionDecs [] = []
functionDecs ((StmtLine (Global (Assign (Var v) _))):ss) = [v] ++ functionDecs ss
functionDecs ((StmtLine (Function v _ _)):ss) = [v] ++ functionDecs ss
functionDecs (s:ss) = functionDecs ss

maybeNamed :: String -> NameSpace -> Maybe String
maybeNamed s [] = Nothing
maybeNamed s ((n,vs):ns) = if elem s vs then Just n else maybeNamed s ns

imprts :: [StmtLine] -> [(String,String)]
imprts ss = case ss of
  (StmtLine (Where ds)):rest -> (imprtsAux ds) ++ (imprts rest)
  _:rest -> imprts rest
  [] -> []

imprtsAux :: [DefLine] -> [(String,String)]
imprtsAux ds = case ds of
  (DefLine (Import x y)):rest -> [(x,y)] ++ (imprtsAux rest)
  _:rest -> imprtsAux rest
  [] -> []

--eof
