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
       imps <- return $ imprts ss
       setModule m
       expandNameSpace [(m, functionDecs ss)]
       raw "import uxadt"
       newline
       raw "import Informl"
       newline
       compileImps imps
       raw "Informl = Informl.Informl"
       newline
       raw $ defineQual qual
       raw $ "class " ++ m ++ ":"
       newline
       raw "  "
       raw $ defineUnqual unqual
       newline
       indent
       mapM compile ss
       unindent
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
       --  raw if patIsEmp p then "#" else ""
         raw $ "  " ++ (patternTups [p]) ++ " = "
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

    Set v e wb ->
      do m <- getModule
         tmp <- freshWithPrefix "__iml"
         bool <- freshWithPrefix "__iml_enterd"
         othw <- return $ hasOtherwise wb
         othwv <- return $ map (otherwiseLine v) othw 
         err <- if othw == [] then return $ Block [StmtLine (StmtExp (Assign (Var bool) CFalse)) , StmtLine (Throws (Literal "Pattern Missmatch"))] 
                  else return (Block (StmtLine (StmtExp (Assign (Var bool) CFalse)):othwv))
         raw $ tmp ++ " = "
         compile e
         newline
         raw $ v ++ " = " ++ tmp
         mapM compilePatCheck wb
         raw ".end"
         newline
         raw $ bool ++ " = True"
         newline
         compile (If (Neq (Var v) (Int 1)) err)
         compile (If (Var bool) (Block (map (setWhen tmp v) wb)))

    Get e wb ->
      do eval <- freshWithPrefix "__iml"
         tmp <- freshWithPrefix "__iml"
         m <- getModule
         othw <- return $ hasOtherwise wb
         err <- if othw == [] then return $ Block [StmtLine (Throws (Literal "Pattern Missmatch"))] 
                  else return (Block othw)
         raw $ eval ++ " = "
         compile e
         newline
         raw $ tmp ++ " = " ++ eval
         mapM compilePatCheck wb
         raw ".end"
         newline
         compile (If (Neq (Var tmp) (Int 1)) err)
         mapM (compileWhen eval) wb
         newline

    For e b -> do {raw "for "; compile e; raw ":"; compile b}
    While e b -> do {raw "while "; compile e; raw ":"; compile b}
    If e b -> do {raw $ "if "; compile e; raw ":"; compile b}
    ElseIf e b -> do {raw $ "elif "; compile e; raw ":"; compile b}
    Else b -> do {raw $ "else:"; compile b}
    Global e -> do {string "global "; compile e; string ";"}
    Local e -> do {compile e; raw "# Local variable."}
    Return e -> do {string "return "; compile e}
    Continue -> do string "continue"
    Break -> do string "break"
    StmtExp e -> do { compile e }
    Throws e -> do {raw "raise NameError("; compile e; raw ")"}
    Where _ -> do nothing

instance ToPython Exp where
  compile e = case e of
    Var v -> 
      do ns <- getNameSpace 
         string $ maybe v (\x-> x ++ "." ++ v) $ maybeNamed v ns
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
    Is e p       -> do {raw "uxadt.M("; compile e; raw ", "; compile p; raw ")"}
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

    Bars e       -> do {raw $ "informl.size("; compile e; raw ")"}

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
      do ns <- getNameSpace
         raw $ maybe v (\x-> x ++ "." ++ v) $ maybeNamed v ns
         raw "(" 
         compileIntersperse ", " es
         raw ")"

    Lambda ss e ->
      do raw $ "lambda " ++ (join "," ss) ++ " : "
         compile e

    Dot (ConApp c []) f -> do {raw c; raw "."; compile f}

    Tuple es ->
      do raw $ if length es < 7 then "anonymous.anonymous_" ++ (show (length es)) ++ "(" else "["
         compileIntersperse ", " es
         raw $ if length es < 7 then ")" else "]"
    
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
    WhenBlock p b -> StmtLine (If (Is (Var var) p) (Block (map (otherwiseLine val) b)))
    a -> StmtLine (StmtExp (Literal "ENDOFSET")) 

compileWhen :: String -> WhenBlock -> Compilation ()
compileWhen v wb = case wb of 
    WhenBlock p b ->
      do compile (If (Is (Var v) p) (Block b))
    _ -> do nothing

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
  a -> a

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

compileImps :: [(String,String)] -> Compilation ()
compileImps [] = do nothing
compileImps ((x,y):rest) = do raw $ "import " ++ y ++ " as " ++ x
                              newline
                              compileImps rest

--eof