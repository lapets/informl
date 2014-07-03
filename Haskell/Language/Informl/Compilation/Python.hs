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
       raw "import uxadt\n"
       raw "_ = None\n"
       raw "import Informl\n"
       raw "informl = Informl.Informl()\n"
       raw $ defineQual qual
       raw $ "class " ++ m ++ ":\n"
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
         raw $ "  " ++ (patternTups [p]) ++ " = "
         compile e
         compile b

    ElseIf (Is e p) b ->
      do raw $ "elif "
         compile e
         raw " < "
         compile p
         raw ":"
         newline
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
    Where _ -> do nothing

instance ToPython Exp where
  compile e = case e of
    Var v        -> do string v
    CTrue        -> do string "True"
    CFalse       -> do string "False"
    CNothing     -> do string "None"
    Int n        -> do string $ show n
    ConApp c es  ->
      do raw c
         raw "("
         compileIntersperse ", " es
         raw ")"

    Concat e1 e2   -> do {compile e1; raw " + "; compile e2}

    Pow   e1 e2  -> do {raw "("; compile e1; raw "**"; compile e2; raw ")"}
    Mult  e1 e2  -> do {raw "("; compile e1; raw " * "; compile e2; raw ")"}
    Div   e1 e2  -> do {raw "informl.div("; compile e1; raw ", "; compile e2; raw ")"}
    Plus  e1 e2  -> do {raw "informl.plus("; compile e1; raw ", "; compile e2; raw ")"}
    Minus e1 e2  -> do {raw "("; compile e1; raw " - "; compile e2; raw ")"}

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

    Assign e1 e2 -> do {compile e1; raw " = "; compile e2}

    Bars e       -> do {raw $ "informl.size("; compile e; raw ")"}

    Bracks (Tuple es) -> do {raw "["; compileIntersperse ", " es; raw "]"}
    Bracks e -> do {raw "["; compile e; raw "]"}
    ListItem c e -> do {compile c; raw "["; compile e; raw "]"}

    FunApp v es  -> 
      do m <- getModule
         string (if inStdlib v then "informl." ++ v else (maybe "informl" id m) ++ "." ++ v)
         raw "(" 
         compileIntersperse ", " es
         raw ")"
    
    _ -> do string "None"
    
instance ToPython Pattern where
  compile p = case p of
    PatternVar v    -> do raw v
    PatternCon c ps ->
      do qual <- maybeQualified c
         qualEnv <- getQualEnv
         raw $ (maybe c (\x -> (x ++ "." ++ c)) qual) ++ "(" ++ underscores ps qualEnv ++ ")"

patternTups :: [Pattern] -> String
patternTups [] = ""
patternTups [PatternVar v] = v
patternTups ((PatternVar v):ps)   = v ++ "," ++ patternTups ps
patternTups [PatternCon c ps]     = "(" ++ patternTups ps ++ ")"
patternTups ((PatternCon c ps):xs)= "(" ++ patternTups ps ++ ")," ++ patternTups xs 

compilePatternVars :: String -> Pattern -> [String]
compilePatternVars tmp p = case p of
  PatternVar v    -> [v ++ " = " ++ tmp ++ "[\"" ++ v ++ "\"];"]
  PatternCon c ps -> concat $ map (compilePatternVars tmp) ps

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
underscores [PatternVar v] qe = "_"
underscores ((PatternVar v):xs) qe = "_," ++ underscores xs qe

defAux :: [(String,Int)] -> String
defAux [] = ""
defAux [(s,i)] = "\'" ++ s ++ "\':[" ++ defUnderscores i ++ "]"
defAux ((s,i):xs) = "\'" ++ s ++ "\':[" ++ defUnderscores i ++ "]," ++ (defAux xs)

defUnderscores :: Int -> String
defUnderscores 0 = ""
defUnderscores 1 = "_"
defUnderscores i = "_," ++ defUnderscores (i-1)


defineUnqual :: [(Constructor,Int)] -> String
defineUnqual ps = if ps == []  then "" else "uxadt._({" ++ defAux ps ++ "})\n"

defineQual :: QualEnv -> String
defineQual []          = ""
defineQual ((q,ps):qs) = if ps == [] then defineQual qs 
                         else "uxadt.qualified(\'" ++ q ++ "\', {" ++ defAux ps ++ "})\n"
                              ++ defineQual qs

--eof