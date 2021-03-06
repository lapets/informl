----------------------------------------------------------------
--
-- Informl
-- Meta-language for quickly defining DSLs, including their
-- abstract syntax data structures, parsers, prettyprinters,
-- and other transformations.
--
-- Language/Informl/Compilation/Python.hs
--   Haskell implementation of the Informl language Python
--   compilation algorithm.
--

----------------------------------------------------------------
-- 

module Language.Informl.Compilation.Python
  where

import Data.String.Utils (join)
import Data.List.Utils (replace)

import Language.Informl.Compilation
import Language.Informl.AbstractSyntax

----------------------------------------------------------------
-- 

class ToPython a where
  compile :: a -> Compilation ()

instance ToPython Top where
  compile (Top m) = compile m

instance ToPython Module where
  compile (Module m ss) = 
    do unqual <- setQualEnv ss
       qual <- getQualEnv
       setModule m
       expandNameSpace [(m, functionDecs ss)]
       raw "import uxadt"
       newline
       raw "import Informl"
       newline
       newline
       raw $ defineQual qual
       newline
       raw $ defineUnqual unqual
       newline
       mapM compile ss
       newline

instance ToPython StmtLine where
  compile (StmtLine s) = do {newline; compile s}

instance ToPython Block where
  compile b = case b of
    Stmt s -> do compile s
    Block ss ->
      do indent
         mapM compile ss
         unindent
         newline

instance ToPython Stmt where
  compile s = case s of

    Import m a -> do raw $ "import " ++ m ++ " as " ++ a

    If (Is e p) b ->
      do raw $ "if "
         compile e
         raw " < "
         compile p
         raw ":"
         newline
         raw $ if patIsEmp p then "#" else ""
         raw $ "  (" 
         compilePatternVars [p] 
         raw ",) = "
         compile e
         compile b

    ElseIf (Is e p) b ->
      do raw $ "elif "
         compile e
         raw " < "
         compile p
         raw ":"
         newline
         raw $ if patIsEmp p then "#" else ""
         raw $ "  (" 
         compilePatternVars [p] 
         raw ",) = "
         compile e
         compile b

    Function f xs b ->
      do m <- getModule
         pre <- return $ "def "
         newline
         raw $ pre ++ f ++ " (" ++ join ", " xs ++ "):"
         nest
         compile b
         unnest

    Private f xs b ->
      do m <- getModule
         pre <- return $ "def "
         newline
         raw $ pre ++ f ++ " (" ++ join ", " xs ++ "):"
         nest
         compile b
         unnest

    Set v e wb ->
      do tmp <- freshWithPrefix "__iml"
         othw <- return $ hasOtherwise wb
         err <- return $ StmtLine (Else (Block [StmtLine (Throws (Literal "Pattern Missmatch"))]))
         raw $ tmp ++ " = "
         compile e
         newline
         compile (StmtLine ((If (CFalse) (Block [StmtLine(Return CNothing)]))))
         mapM compile (map (setWhen tmp v) wb)
         if othw == [] then compile err else nothing

    Get e wb ->
      do eval <- freshWithPrefix "__iml"
         othw <- return $ hasOtherwise wb
         err <- return $ Else (Block [StmtLine (Throws (Literal "Pattern Missmatch"))])
         raw $ eval ++ " = "
         compile e
         newline
         compile (StmtLine((If (CFalse) (Block [StmtLine(Return CNothing)]))))
         mapM (compileWhen eval) wb
         if othw == [] then compile err else nothing

    For e b -> do {raw "for "; compile e; raw ":"; compile b}
    While e b -> do {raw "while "; compile e; raw ":"; compile b}
    If e b -> do {raw $ "if "; compile e; raw ":"; compile b}
    ElseIf e b -> do {raw $ "elif "; compile e; raw ":"; compile b}
    Else b -> do {raw $ "else:"; compile b}
    Global e -> do compile e
    Local e -> do {compile e; raw "# Local variable."}
    Return e -> do {string "return "; compile e}
    Continue -> do string "continue"
    Break -> do string "break"
    StmtExp e -> do { compile e }
    Throws e -> do {raw "raise NameError("; compile e; raw ")"}
    Where _ -> do nothing

instance ToPython Exp where
  compile e = case e of
    Var v        -> do string v 
    CTrue        -> do string "True"
    CFalse       -> do string "False"
    CNothing     -> do string "None"
    Int n        -> do string $ show n
    Float f      -> do string $ show f
    Literal s    -> do {raw $ "\"" ++ (compileLiteral s) ++ "\""}
    ConApp c es  ->
                do qual <- maybeQualified c
                   raw $ (maybe c (\x-> x ++ "." ++c) qual) ++ "("
                   compileIntersperse ", " es
                   raw ")"
    Neg (Int i)  -> do {raw $ "-" ++ (show i);}
    Neg (Float f)-> do {raw $ "-" ++ (show f);}

    Concat e1 e2   -> do {compile e1; raw " + "; compile e2}

    Pow   e1 e2  -> do {raw "("; compile e1; raw "**"; compile e2; raw ")"}
    Mult  e1 e2  -> do {raw "("; compile e1; raw " * "; compile e2; raw ")"}
    Div   e1 e2  -> do {raw "("; compile e1; raw " / "; compile e2; raw ")"}
    Plus  e1 e2  -> do {raw "Informl.plus("; compile e1; raw ", "; compile e2; raw ")"}
    Minus e1 e2  -> do {raw "Informl.minus("; compile e1; raw ", "; compile e2; raw ")"}

    Eq  e1 e2    -> do {compile e1; raw " == "; compile e2}
    Neq e1 e2    -> do {compile e1; raw " != "; compile e2}
    Lt  e1 e2    -> do {compile e1; raw " < "; compile e2}
    Leq e1 e2    -> do {compile e1; raw " <= "; compile e2}
    Gt  e1 e2    -> do {compile e1; raw " > "; compile e2}
    Geq e1 e2    -> do {compile e1; raw " >= "; compile e2}

    And e1 e2    -> do {raw "("; compile e1; raw " and "; compile e2; raw ")"}
    Or  e1 e2    -> do {raw "("; compile e1; raw " or "; compile e2; raw ")"}
    Not e        -> do {raw "(not("; compile e; raw "))"}

    In e1 e2     -> do {compile e1; raw " in "; compile e2}
    Is e p       -> do { compile e; raw " < "; compile p}
    Subset e1 e2 -> do {compile e1; raw ".subset("; compile e2; raw ")"}

    Maps f l -> do {raw "Informl.map("; compile l; raw ", "; compile f; raw ")"}
    Folds f (On b l) -> 
      do raw "Informl.fold("
         compile l
         raw ", " 
         compile f
         raw ", "
         compile b
         raw ")"

    Assign e1 e2 -> do {compile e1; raw " = "; compile e2}
    PlusAssign e1 e2 -> 
      do  compile e1
          raw " = Informl.plus("
          compile e1
          raw ", "
          compile e2
          raw ")"
    MinusAssign e1 e2 ->
      do  compile e1
          raw " = Informl.minus("
          compile e1
          raw ", "
          compile e2
          raw ")"
    MultAssign e1 e2 -> do {compile e1; raw " *= "; compile e2}
    DivAssign e1 e2 -> do {compile e1; raw " /= "; compile e2}

    Bars e       -> do {raw $ "Informl.size("; compile e; raw ")"}

    Bracks (Tuple es) -> do {raw "["; compileIntersperse ", " es; raw "]"}
    Bracks e -> do {raw "["; compile e; raw "]"}
    Braces (Tuple es) -> do {raw "{"; compileIntersperse ", " es; raw "}"}
    Braces e -> do {raw "{"; compile e; raw "}"}
    IndexItem c [(DictItem d (Var "_"))] -> 
      do compile c
         raw "["
         compile d
         raw ":]"
    IndexItem c [(DictItem (Var "_") d)] ->
      do compile c
         raw "[:"
         compile d 
         raw "]"
    IndexItem c [(DictItem e f)] ->
      do compile c
         raw "["
         compile e
         raw ":"
         compile f
         raw "]"
    ListItem c e -> do {compile c; raw "["; compile e; raw "]"}
    DictItem k v -> do {compile k; raw ":"; compile v}
    IndexItem a es -> do {compile a; raw "["; compileIntersperse "][" es; raw "]"}

    FunApp v es  -> 
      do raw $ if inStdlib v then "Informl." ++ v else v
         raw "(" 
         compileIntersperse ", " es
         raw ")"

    Lambda ss e ->
      do raw $ "lambda " ++ (join "," ss) ++ " : "
         compile e

    Dot (ConApp c []) f -> do {raw c; raw "."; compile f}
    Dot c f -> do {compile c; raw "."; compile f}

    Tuple es ->
      do raw "("
         compileIntersperse ", " es
         raw ")"

    IfX e1 (ElseX e2 e3) ->
      do compile (IfExp e1 e2 e3)
    
    IfX e1 e2 ->
      do compile (IfExp e1 e2 CNothing)

    ElseX e1 e2 ->
      do compile (IfExp e1 (Or (Eq e1 CNothing) (Eq e1 CFalse)) e2)

    IfExp (Assign a t) e f ->
      do compile $ Assign a $ IfExp t e f

    IfExp (PlusAssign a t) e f ->
      do compile $ PlusAssign a $ IfExp t e f

    IfExp (MinusAssign a t) e f ->
      do compile $ MinusAssign a $ IfExp t e f

    IfExp (MultAssign a t) e f ->
      do compile $ MultAssign a $ IfExp t e f

    IfExp (DivAssign a t) e f ->
      do compile $ DivAssign a $ IfExp t e f

    IfExp t (Is e p) f ->
      do compile e
         raw "._("
         compile p
         raw ", lambda "
         compilePatternVars [p]
         raw " : "
         compile t
         raw ").end if "
         compile (Is e p)
         raw " else "
         compile f
         
    IfExp t e f ->
      do compile t
         raw " if "
         compile e
         raw " else "
         compile f
    
    _ -> do string "None"
    
instance ToPython Pattern where
  compile p = case p of
    PatternVar v    -> do raw v
    PatternCon c ps ->
      do qual <- maybeQualified c
         qualEnv <- getQualEnv
         raw $ (maybe c (\x -> (x ++ "." ++ c)) qual) ++ "(" ++ underscores ps qualEnv ++ ")"

setWhen :: String -> String -> WhenBlock -> StmtLine
setWhen var val wb = case wb of 
    WhenBlock p b -> StmtLine (ElseIf (Is (Var var) p) (Block (map (otherwiseLine val) b)))
    Otherwise b -> StmtLine (Else (Block (map (otherwiseLine val) b)))

compileWhen :: String -> WhenBlock -> Compilation ()
compileWhen v wb = case wb of 
    WhenBlock p b ->
      do compile (ElseIf (Is (Var v) p) (Block b))
    Otherwise b -> do compile (Else (Block b))

compilePatCheck :: WhenBlock -> Compilation ()
compilePatCheck wb = case wb of
  WhenBlock p b ->  do raw "._("
                       compile p
                       raw ", lambda "
                       compilePatternVars [p]
                       raw ": 1)"
  _ -> do nothing

compilePatternVars :: [Pattern] -> Compilation ()
compilePatternVars xs = case xs of
  [] -> do nothing
  [PatternVar v] -> do raw v
  (PatternVar v):ps  -> do {raw $ v ++ ","; compilePatternVars ps}
  [PatternCon c ps] -> do compilePatternVars ps
  (PatternCon c ps):ys -> do compilePatternVars ps 
                             if (length ps) > 0 then raw "," else nothing
                             compilePatternVars ys
  [AnonPattern ps] -> do compilePatternVars ps
  (AnonPattern ps):ys -> do {compilePatternVars ps; raw ","; compilePatternVars ys} 

otherwiseLine :: String -> StmtLine -> StmtLine
otherwiseLine s sl = case sl of 
  StmtLine (Value e) -> StmtLine (StmtExp (Assign (Var s) e))
  StmtLine (Function n vs (Block ss)) -> StmtLine (Function n vs (Block (map (otherwiseLine s) ss)))
  StmtLine (Private n vs (Block ss)) -> StmtLine (Private n vs (Block (map (otherwiseLine s) ss)))
  StmtLine (If e (Block ss)) -> StmtLine (If e (Block (map (otherwiseLine s) ss)))
  StmtLine (Else (Block ss)) -> StmtLine (Else (Block (map (otherwiseLine s) ss)))
  StmtLine (ElseIf e (Block ss)) -> StmtLine (ElseIf e (Block (map (otherwiseLine s) ss)))
  StmtLine (While e (Block ss)) -> StmtLine (While e (Block (map (otherwiseLine s) ss)))
  StmtLine (Get e wb) -> StmtLine (Get e (map (otherwiseLineWhen s) wb))
  a -> a

otherwiseLineWhen ::  String -> WhenBlock -> WhenBlock
otherwiseLineWhen s sl = case sl of 
  WhenBlock p ss -> WhenBlock p (map (otherwiseLine s) ss)
  Otherwise ss -> Otherwise (map (otherwiseLine s) ss)


patternTups :: [Pattern] -> String
patternTups [] = ""
patternTups ((PatternVar v):ps)   = v ++ "," ++ patternTups ps
patternTups [PatternCon c ps]     = if length ps == 0 then "" else "(" ++ patternTups ps ++ ")"
patternTups ((PatternCon c ps):xs)= (if length ps == 0 then "" else "(" ++ patternTups ps ++ "),") ++ patternTups xs

patIsEmp :: Pattern -> Bool
patIsEmp p = case p of
  PatternCon _ ps -> length ps == 0 || foldr (&&) True (map patIsEmp ps)
  PatternVar _ -> False

compileIntersperse :: ToPython a => String -> [a] -> Compilation ()
compileIntersperse s xs = case xs of
  []   -> do nothing
  [x]  -> do compile x
  x:xs -> do {compile x; raw s; compileIntersperse s xs }

underscores :: [Pattern] -> QualEnv -> String
underscores [] qe = ""
underscores [PatternCon c ps] qe = let q = maybeQualifiedAux c qe in 
      (maybe "" (\x->x++".") q) ++ c ++ "(" ++ underscores ps qe ++ ")"
underscores ((PatternCon c ps):xs) qe = let q = maybeQualifiedAux c qe in
      (maybe "" (\x->x++".") q) ++ c ++ "(" ++ underscores ps qe ++ ")," ++ underscores xs qe
underscores [PatternVar v] qe = "None"
underscores ((PatternVar v):xs) qe = "None," ++ underscores xs qe

defAux :: [(String,Int)] -> String
defAux [] = ""
defAux [(s,i)] = "\'" ++ s ++ "\':[" ++ defUnderscores i ++ "]"
defAux ((s,i):xs) = "\'" ++ s ++ "\':[" ++ defUnderscores i ++ "]," ++ (defAux xs)

defUnderscores :: Int -> String
defUnderscores 0 = ""
defUnderscores 1 = "None"
defUnderscores i = "None," ++ defUnderscores (i-1)


defineUnqual :: [(Constructor,Int)] -> String
defineUnqual ps = if ps == []  then "" else "uxadt._({" ++ defAux ps ++ "})\n"

defineQual :: QualEnv -> String
defineQual []          = ""
defineQual ((q,ps):qs) = if ps == [] then defineQual qs 
                         else "uxadt.qualified(\'" ++ q ++ "\', {" ++ defAux ps ++ "})\n"
                              ++ defineQual qs

--eof