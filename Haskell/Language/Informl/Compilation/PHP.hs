----------------------------------------------------------------
--
-- Informl
-- Meta-language for quickly defining DSLs, including their
-- abstract syntax data structures, parsers, prettyprinters,
-- and other transformations.
--
-- Language/Informl/Compilation/PHP.hs
--   Haskell implementation of the Informl language PHP
--   compilation algorithm.
--

----------------------------------------------------------------
-- 

module Language.Informl.Compilation.PHP
  where

import Data.String.Utils (join)
import Data.List.Utils (replace)

import Language.Informl.Compilation
import Language.Informl.AbstractSyntax

----------------------------------------------------------------
-- 

class ToPHP a where
  compile :: a -> Compilation ()

instance ToPHP Top where
  compile (Top m) = compile m

instance ToPHP Module where
  compile (Module m ss) = 
    do setModule m
       expandNameSpace [(m, functionDecs ss)]
       raw $ "<?php namespace " ++ m ++ ";\ninclude \"uxadt.php\";\ninclude \"Informl.php\";\n"
       unqual <- setQualEnv ss
       newline
       raw $ defineQual $ getQualifiedNow ss
       newline
       indent
       raw $ defineUnqual unqual
       newline
       mapM compile ss
       unindent
       newline
       raw " ?>"

instance ToPHP StmtLine where
  compile (StmtLine s) = do {newline; compile s}

instance ToPHP Block where
  compile b = case b of 
    Stmt s -> do compile s
    Block ss ->
      do indent
         mapM compile ss
         unindent
         newline

instance ToPHP Stmt where
  

  compile s = case s of

    Import m a ->
      do expandNameSpace [(m,[a])]

    For (In e1 e2) b ->
      do lst <- freshWithPrefix "__iml"
         raw "foreach ("
         compile e2; raw "->arr as "
         compile e1; raw "){"
         newline 
         compile b 
         raw "}"
         newline

    Function f xs b ->
      do d <- depth
         newline
         if (d == 0) 
           then raw $ "function " ++ f ++ "(" ++ (if xs == [] then "" else "$" ++ join ", $" xs) ++ ") {"
           else raw $ "$" ++ f ++ " = " ++ "function(" ++ (if xs == [] then "" else "$" ++ join ", $" xs) ++ ") {"
         nest
         compile b
         unnest
         string "}"

    Private f xs b ->
      do d <- depth
         newline
         if (d == 0) 
           then raw $ "private function " ++ f ++ "(" ++ (if xs == [] then "" else "$" ++ join ", $" xs) ++ ") {"
           else raw $ "$" ++ f ++ " = " ++ "function(" ++ (if xs == [] then "" else "$" ++ join ", $" xs) ++ ") {"
         nest
         compile b
         unnest
         string "}"

    For e b -> do {raw "for "; raw "("; compile e; raw ")"; raw " {"; compile b; raw "}"}
    While e b -> do {raw "while "; raw "("; compile e; raw ") {"; compile b; raw "}"}

    If (Is e p) b ->
      do tmp <- freshWithPrefix "$__iml_is"
         raw $ tmp ++ " = "
         compile e
         raw ";"
         newline
         raw $ "if( " ++ tmp
         raw "->_("
         compile p
         raw ")){"
         newline
         raw $ if patIsEmp p then "//" else ""
         raw "list("
         compilePatternVars [p]
         raw $ ") = " ++ tmp ++ "();"
         compile b
         raw "}"
    ElseIf (Is e p) b ->
      do tmp <- freshWithPrefix "$__iml_is"
         raw $ "else if( "
         compile e
         raw "->_("
         compile p
         raw ")){"
         newline
         raw $ tmp ++ " = "
         compile e
         raw ";"
         newline
         raw $ if patIsEmp p then "//" else ""
         raw "list("
         compilePatternVars [p]
         raw $ ") = " ++ tmp ++ "();"
         compile b
         raw "}"


    If e b -> do {raw $ "if ("; compile e; raw ") {"; compile b; raw "}"}
    ElseIf e b -> do {raw $ "else if ("; compile e; raw ") {"; compile b; raw "}"}
    Else b -> do {raw $ "else {"; compile b; raw "}"}
    Global e -> do {string "public "; compile e; string ";"}
    Local e -> do {compile e; string ";"}
    Return e -> do {string "return "; compile e; string ";"}
    Value e -> do {string "return "; compile e; string ";"}
    Continue -> do string "continue;"
    Break -> do string "break;"
    StmtExp e -> do { compile e; string ";" }
    Where _ -> do nothing

    Set v e wb ->
      do tmp <- freshWithPrefix "$__iml"  
         err <- return $ "else throw new \\Exception('Pattern Mismatch');"
         othw <- return $ hasOtherwise wb
         raw $ tmp ++ " = ("
         compile e
         raw $ "); $" ++ v ++ ";" 
         newline
         raw "if(False){return;}"
         newline
         setWhenCompile v tmp wb
         if othw == [] then raw err else nothing

    Get e wb ->
      do eval <- freshWithPrefix "$__iml"
         err <- return $ "else throw new \\Exception('Pattern Mismatch');"
         othw <- return $ hasOtherwise wb
         raw $ eval ++ " = "
         raw "("
         compile e
         raw ");"
         newline
         raw "if(False){return;}"
         newline
         whenCompile eval wb
         if othw == [] then raw err else nothing

instance ToPHP Exp where
  compile e = case e of
    Var v -> 
      do ns <- getNameSpace 
         string $ maybe ("$" ++ v) (\x-> "$" ++ x ++ "->" ++ v) $ maybeNamed v ns
    CTrue        -> do string "True"
    CFalse       -> do string "False"
    CNothing     -> do string "NULL"
    Int n        -> do string $ show n
    Float f      -> do string $ show f
    Literal s    -> do string $ "\"" ++ (compileLiteral s) ++ "\""
    ConApp c es  -> do raw $ c ++ "("
                       compileIntersperse ", " es
                       raw ")"
    Neg (Int i)  -> do {raw $ "-" ++ (show i);}
    Neg (Float f)-> do {raw $ "-" ++ (show f);}

    And e1 e2    -> do {raw "("; compile e1; raw " && "; compile e2; raw ")"}
    Or  e1 e2    -> do {raw "("; compile e1; raw " || "; compile e2; raw ")"}
    Not e        -> do {raw "(!("; compile e; raw "))"}

    Concat e1 e2   -> do {compile e1; raw " + "; compile e2}

    Plus e1 e2   -> do {raw "\\Informl\\plus("; compile e1; raw ", "; compile e2; raw ")"}
    Minus e1 e2  -> do {raw "\\Informl\\minus("; compile e1; raw ", "; compile e2; raw ")"}
    Pow   e1 e2  -> do {raw "("; compile e1; raw " ^ "; compile e2;raw ")";}
    Mult  e1 e2  -> do {raw "("; compile e1; raw " * "; compile e2; raw ")"}
    Div   e1 e2  -> do {raw "("; compile e1; raw " / "; compile e2; raw ")"}
    Mod   e1 e2  -> do {raw "("; compile e1; raw " % "; compile e2; raw ")"}

    Eq  e1 e2    -> do {compile e1; raw " == "; compile e2}
    Neq e1 e2    -> do {compile e1; raw " != "; compile e2}
    Lt  e1 e2    -> do {compile e1; raw " < "; compile e2}
    Leq e1 e2    -> do {compile e1; raw " <= "; compile e2}
    Gt  e1 e2    -> do {compile e1; raw " > "; compile e2}
    Geq e1 e2    -> do {compile e1; raw " >= "; compile e2}

    In e1 e2     -> do {raw "\\Informl\\inarray(";compile e1; raw ", " ; compile e2; raw ")"}
    Is e p       -> do {compile e; raw "->_("; compile p; raw ")"}
    Subset e1 e2 -> do {compile e1; raw " subset "; compile e2}

    Assign (IndexItem e es) e2 ->
      do compile e
         raw "->arr["
         compileIntersperse "]->arr[" es
         raw "] = "
         compile e2

    Assign (Tuple es) e2 ->
      do raw "list("
         compileIntersperse ", " es
         raw ") = "
         compile e2

    Assign e1 e2 -> do {compile e1; raw " = "; compile e2}
    PlusAssign e1 e2 -> 
      do  compile e1
          raw " = \\Informl\\plus("
          compile e1
          raw ", "
          compile e2
          raw ")"
    MinusAssign e1 e2 ->
      do  compile e1
          raw " = \\Informl\\minus("
          compile e1
          raw ", "
          compile e2
          raw ")"
    MultAssign e1 e2 -> do {compile e1; raw " *= "; compile e2}
    DivAssign e1 e2 -> do {compile e1; raw " /= "; compile e2}

    Bars e       -> do {raw $ "\\Informl\\size("; compile e; raw ")"}

    Maps f l -> 
      do raw "\\Informl\\map("
         compile l
         raw ", "
         compile f
         raw ")"
    Folds f (On b l) -> 
      do raw "\\Informl\\fold("
         compile l
         raw ", " 
         compile f
         raw ", "
         compile b
         raw ")"

    Bracks (Tuple es) -> do {raw "new \\Informl\\ListIml("; compileIntersperse ", " es; raw ")"}
    Bracks e -> do {raw "new \\Informl\\ListIml("; compile e; raw ")"}
    Braces (Tuple es) -> do {raw "new \\Informl\\Dictionary("; compileIntersperse ", " es; raw ")"}
    Braces e -> do {raw "new \\Informl\\Dictionary("; compile e; raw ")"}
    ListItem c e -> do {compile c; raw "["; compile e; raw "]"}
    DictItem k v -> do {raw "array("; compile k; raw ", "; compile v; raw ")"}

    FunApp v es  -> 
      do raw $ if inStdlib v then "\\Informl\\" ++ v else v
         do {raw "("; compileIntersperse ", " es; raw ")"}

    IndexItem c [(DictItem d (Var "_"))] -> 
      do raw "\\Informl\\slice("
         compile c
         raw ", "
         compile d
         raw ", NULL)"
    IndexItem c [(DictItem (Var "_") d)] ->
      do raw "\\Informl\\slice("
         compile c
         raw ", null,"
         compile d
         raw ")"
    IndexItem c [(DictItem e f)] ->
      do raw "\\Informl\\slice("
         compile c
         raw ", "
         compile e
         raw ", "
         compile f
         raw ")"

    IndexItem e es ->
      do raw $ "( \\Informl\\type("
         compile e
         raw ") == 'string' ? "
         compile e
         raw "["
         compileIntersperse "][" es
         raw "] : "
         compile e
         raw "->arr["
         compileIntersperse "]->arr[" es
         raw "] )"

    Lambda vs e -> 
      do raw $ "function(" 
         raw $ (if length vs == 0 then "" else "$") ++ (join ",$" vs) ++ ")" 
                    ++ (lambdaUses vs e) ++ "{return "
         compile e
         raw ";}"

    IfExp t e f ->
      do raw "("
         compile e
         raw " ? "
         compile t
         raw " : "
         compile f
         raw ")"

    Dot (ConApp c []) f -> 
      do ns <- getNameSpace
         raw "\\"
         raw $ maybe c id $ maybeNamed c ns
         raw "\\"
         compile f
    Dot c f -> do {raw "\\"; compile c; raw "\\"; compile f}

    Tuple es -> do {raw "array("; compileIntersperse ", " es; raw ")"}
    
    _ -> do string "NULL"
    
instance ToPHP Pattern where
  compile p = case p of
    PatternVar v    -> do raw $ "$" ++ v
    PatternCon c ps ->
      do raw $ c ++ "(" ++ (underscores ps)
         raw ")"

instance ToPHP WhenBlock where
  compile wb = case wb of 
    WhenBlock p ss -> 
      do raw "->_("
         compile p
         raw ", function ("
         compilePatternVars [p]
         raw ") {"
         mapM compile ss
         raw "} )"
    Otherwise ss -> do nothing

compilePatCheck :: [WhenBlock] -> Compilation ()
compilePatCheck wb = case wb of
  [] -> do nothing
  ((WhenBlock p b):rest) ->
      do raw "->_("
         compile p
         raw ", function(){return 1;})"
         compilePatCheck rest
  [Otherwise _ ] -> do nothing

whenCompile :: String -> [WhenBlock] -> Compilation ()
whenCompile v wb = case wb of
  (WhenBlock p b):rest -> do tmp <- if v!!0 == '$' then return (tail v) else return v
                             compile (ElseIf (Is (Var tmp) p) (Block b))
                             whenCompile v rest
  (Otherwise b):rest   -> do compile (Else (Block b))
  _ -> do nothing

setWhenCompile :: String -> String -> [WhenBlock] -> Compilation ()
setWhenCompile val pv wb = case wb of
  (WhenBlock p b):rest -> do tmp1 <- if val!!0 == '$' then return (tail val) else return val
                             tmp2 <- if pv!!0 == '$' then return (tail pv) else return pv
                             compile (ElseIf (Is (Var tmp2) p) (Block (map (valLine tmp1) b)))
                             setWhenCompile val pv rest
  (Otherwise b):rest   -> do tmp1 <- if val!!0 == '$' then return (tail val) else return val
                             compile (Else (Block (map (valLine tmp1) b)))
  _ -> do nothing

valLine :: String -> StmtLine -> StmtLine
valLine s sl = case sl of 
  StmtLine (Value e) -> StmtLine (StmtExp (Assign (Var s) e)) 
  StmtLine (Function n vs (Block ss)) -> StmtLine (Function n vs (Block (map (valLine s) ss)))
  StmtLine (Private n vs (Block ss)) -> StmtLine (Private n vs (Block (map (valLine s) ss)))
  StmtLine (If e (Block ss)) -> StmtLine (If e (Block (map (valLine s) ss)))
  StmtLine (Else (Block ss)) -> StmtLine (Else (Block (map (valLine s) ss)))
  StmtLine (ElseIf e (Block ss)) -> StmtLine (ElseIf e (Block (map (valLine s) ss)))
  StmtLine (While e (Block ss)) -> StmtLine (While e (Block (map (valLine s) ss)))
  StmtLine (Get e wb) -> StmtLine (Get e (map (valLineWhen s) wb))
  a -> a

valLineWhen ::  String -> WhenBlock -> WhenBlock
valLineWhen s sl = case sl of 
  WhenBlock p ss -> WhenBlock p (map (valLine s) ss)
  Otherwise ss -> Otherwise (map (valLine s) ss)

compileOtherwiseBlock :: String -> [StmtLine] ->Compilation ()
compileOtherwiseBlock v ss = case ss of
  ((StmtLine (Value e)):ss) -> 
                  do raw $  "$" ++ v ++ " = "
                     compile e
                     raw ";"
  (s:ss) -> do compile s
               compileOtherwiseBlock v ss
  [] -> do nothing

patIsEmp :: Pattern -> Bool
patIsEmp p = case p of
  PatternCon _ ps -> length ps == 0 || foldr (&&) True (map patIsEmp ps)
  PatternVar _ -> False

compilePatternVars :: [Pattern] -> Compilation ()
compilePatternVars xs = case xs of
  [] -> do nothing
  [PatternVar v] -> do raw $ "$" ++ v
  (PatternVar v):ps  -> do {raw $ "$" ++ v ++ ","; compilePatternVars ps}
  [PatternCon c ps] -> do compilePatternVars ps
  (PatternCon c ps):ys -> do {compilePatternVars ps; if length ps /= 0 then raw "," else nothing; compilePatternVars ys}

compileIntersperse :: ToPHP a => String -> [a] -> Compilation ()
compileIntersperse s xs = case xs of
  []   -> do nothing
  [x]  -> do compile x
  x:xs -> do {compile x; raw s; compileIntersperse s xs }

underscores :: [Pattern] -> String
underscores [] = ""
underscores [PatternCon c ps] = c ++ "(" ++ (underscores ps) ++ ")"
underscores ((PatternCon c ps):xs) = c ++ "(" ++ (underscores ps) ++ ")," ++ (underscores xs)
underscores [PatternVar v] = "NULL"
underscores (PatternVar v:xs) = "NULL," ++ underscores xs

nestPatterns :: [StmtLine] -> Compilation ()
nestPatterns ss = case ss of
  [] -> do raw "end;\n"
  StmtLine (If (Is e (PatternCon c ps)) b):ss -> 
    do raw $ "_(" ++  c ++ "(" ++ underscores ps ++ ")" ++ ", function(" 
       compilePatternVars ps 
       raw ") {"
       compile b
       raw $ "})\n->"
       nestPatterns ss
  StmtLine (ElseIf (Is e (PatternCon c ps)) b):ss ->
    do raw $ "_(" ++  c ++ "(" ++ underscores ps ++ ")" ++ ", function("
       compilePatternVars ps 
       raw ") {"
       compile b
       raw $ "})\n->"
       nestPatterns ss
  ss -> do {raw "end;\n"; compile (Block ss)}

defineUnqual :: [(Constructor,Int)] -> String
defineUnqual ps = if ps == []  then "" else "\\uxadt\\_(array(" ++ defAux ps ++ "));\n"

defineQual :: QualEnv -> String
defineQual []          = ""
defineQual ((q,ps):qs) = if ps == [] then defineQual qs 
                         else "\\uxadt\\_(\'" ++ q ++ "\', array(" ++ defAux ps ++ "));\n"
                              ++ defineQual qs

defAux :: [(String,Int)] -> String
defAux [] = ""
defAux [(s,i)] = "\'" ++ s ++ "\'=>array(" ++ defUnderscores i ++ ")"
defAux ((s,i):xs) = "\'" ++ s ++ "\'=>array(" ++ defUnderscores i ++ ")," ++ (defAux xs)

defUnderscores :: Int -> String
defUnderscores 0 = ""
defUnderscores 1 = "NULL"
defUnderscores i = "NULL," ++ defUnderscores (i-1)

lambdaUses :: [String] -> Exp -> String
lambdaUses ss e = let l = lambdaUsesVars ss e in 
                    if l == [] then "" 
                      else " use (&$" ++ (join ",&$" (lambdaUsesVars ss e)) ++ ") "

lambdaUsesVars :: [String] -> Exp -> [String]
lambdaUsesVars ss ex = case ex of
 Var v -> if elem v ss then [] else [v]
 (ConApp s es) -> foldr (++) [] (map (lambdaUsesVars ss) es)
 (And e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (Or e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (In e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (Subset e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (Eq e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (Neq e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (Lt e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (Gt e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (Leq e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (Geq e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (Union e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (Intersect e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (Max e) -> lambdaUsesVars ss e
 (Min e) -> lambdaUsesVars ss e
 (Domain e) -> lambdaUsesVars ss e
 (Codomain e) -> lambdaUsesVars ss e
 (Concat e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (Plus e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (Minus e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (Mult e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (Div e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (Pow e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (Not e) -> lambdaUsesVars ss e
 (Neg e) -> lambdaUsesVars ss e
 (Dot _ f) -> lambdaUsesVars ss f -- skips the module reference
 (Parens e) -> lambdaUsesVars ss e
 (Bracks e) -> lambdaUsesVars ss e
 (Braces e) -> lambdaUsesVars ss e
 (Bars e) -> lambdaUsesVars ss e
 (FunApp v es) -> foldr (++) [] (map (lambdaUsesVars ss) es) ++ (if elem v ss then [] else [v])
 (Tuple es) -> foldr (++) [] (map (lambdaUsesVars ss) es)
 (ListItem e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (DictItem k v) -> lambdaUsesVars ss k ++ lambdaUsesVars ss v
 (Maps e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (Folds e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (On e f) -> lambdaUsesVars ss e ++ lambdaUsesVars ss f
 (IndexItem e es) -> lambdaUsesVars ss e ++ foldr (++) [] (map (lambdaUsesVars ss) es)
 _ -> []
                                        
                                        

--eof
