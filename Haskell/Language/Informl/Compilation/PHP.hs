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
       raw $ "<?php\ninclude \"uxadt.php\";\ninclude \"Informl.php\";\ndefine('_', null);\n"
       unqual <- setQualEnv ss
       newline
       raw $ defineQual $ getQualifiedNow ss
       raw $ "\\uxadt\\qualified(\'" ++ m ++ "_ERROR\',{PatternMismatch:[]});"
       newline
       raw $ "class " ++ m ++ "{"
       indent
       raw $ defineUnqual unqual
       newline
       mapM compile ss
       unindent
       newline
       raw "} ?>"

instance ToPHP StmtLine where
  compile (StmtLine s) = do {newline; compile s}

instance ToPHP Block where
  compile b = case b of
    Stmt (If (Is e (PatternCon c ps)) b ) ->
      do tmp <- freshWithPrefix "$__iml"
         raw $ tmp ++ "="
         compile e
         raw ";"
         newline
         raw $ "return " ++ tmp ++ "->"
         raw $ "_(" ++  c ++ "(" ++ underscores ps ++ ")"
         raw ", function("
         compilePatternVars ps 
         raw ") {"
         compile b
         raw "})->end();"
         newline
    Block (StmtLine (If (Is e (PatternCon c ps)) b ) : ss) ->
      do tmp <- freshWithPrefix "$__iml"
         raw $ tmp ++ "="
         compile e
         raw ";"
         newline
         raw $ "return " ++ tmp
         raw $ "\n->_(" ++  c ++ "(" ++ underscores ps ++ ")"
         raw ", function(" 
         compilePatternVars ps 
         raw ") {"
         compile b
         raw "})\n->"
         nestPatterns ss
         newline 
    Stmt s -> do compile s
    Block ss ->
      do indent
         mapM compile ss
         unindent
         newline

instance ToPHP Stmt where
  

  compile s = case s of

    For (In e1 e2) b ->
      do {raw "foreach ("; compile e2; raw " as "; compile e1; raw "){\n"; compile b ; raw "\n}"}

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

    For e b -> do {raw "for "; raw "("; compile e; raw ")"; raw " {"; compile b; raw "}"}
    While e b -> do {raw "while "; raw "("; compile e; raw ") {"; compile b; raw "}"}
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
      do m <- getModule
         err <- return $ "return " ++ (maybe "" (\x -> "$" ++ x) m) ++ "_ERROR->PatternMismatch();"
         iff <- return $ "if(!isset( $" ++v ++" )) "
         ret <- return $ iff ++ err
         othw <- return $ hasOtherwise wb
         raw $ "$"++ v ++ "; " ++ "try { $" ++ v ++ " = "
         raw "("
         compile e
         raw ")"
         mapM compile wb
         raw $ if othw /= [] then iff ++ "{"
                else "->end;}catch(Exception $e){" ++ err ++ "} " ++ ret
         mapM compile othw
         raw $ if othw /= [] then "}"
                else ""

    Get e wb ->
      do tmp <- freshWithPrefix "$__iml"
         m <- getModule
         err <- return $ "return " ++ (maybe "" (\x->"$"++x) m) ++ "_ERROR->PatternMismatch();"
         iff <- return $ "if(!isset($" ++ tmp ++" )) "
         ret <- return $ iff ++ err ++ " return " ++ tmp ++ ";"
         othw <- return $ hasOtherwise wb
         raw $ tmp ++ "; " ++ "try {" ++ tmp ++ " = "
         raw "("
         compile e
         raw ")"
         mapM compile wb
         raw $ if othw /= [] then iff ++ "{"
                else "->end;}catch(Exception $e){" ++ err ++ "} " ++ ret
         mapM compile othw
         raw $ if othw /= [] then "} return $" ++ tmp ++ ";"
                else ""

instance ToPHP Exp where
  compile e = case e of
    Var v        -> do {string "$";string v}
    CTrue        -> do string "True"
    CFalse       -> do string "False"
    CNothing     -> do string "NULL"
    Int n        -> do string $ show n
    Float f      -> do string $ show f
    Literal s    -> do string $ "\"" ++ (compileLiteral s) ++ "\""
    ConApp c es  -> do qual <- maybeQualified c
                       raw $ (maybe ("$" ++ c) (\x-> "$" ++ x ++ "->" ++c) qual) ++ "("
                       compileIntersperse ", " es
                       raw ")"
    Neg (Int i)  -> do {raw $ "-" ++ (show i);}
    Neg (Float f)-> do {raw $ "-" ++ (show f);}

    And e1 e2    -> do {raw "("; compile e1; raw " && "; compile e2; raw ")"}
    Or  e1 e2    -> do {raw "("; compile e1; raw " || "; compile e2; raw ")"}
    Not e        -> do {raw "(!("; compile e; raw "))"}

    Concat e1 e2   -> do {compile e1; raw " + "; compile e2}

    Plus e1 e2   -> do {raw "$informl->plus("; compile e1; raw ", "; compile e2; raw ")"}
    Minus e1 e2  -> do {compile e1; raw " - "; compile e2}
    Pow   e1 e2  -> do {raw "$informl->pow("; compile e1; raw ", "; compile e2; raw ")"}
    Mult  e1 e2  -> do {raw "("; compile e1; raw " * "; compile e2; raw ")"}
    Div   e1 e2  -> do {raw "$informl->div("; compile e1; raw ", "; compile e2; raw ")"}
    Mod   e1 e2  -> do {raw "("; compile e1; raw " % "; compile e2; raw ")"}

    Eq  e1 e2    -> do {compile e1; raw " == "; compile e2}
    Neq e1 e2    -> do {compile e1; raw " != "; compile e2}
    Lt  e1 e2    -> do {compile e1; raw " < "; compile e2}
    Leq e1 e2    -> do {compile e1; raw " <= "; compile e2}
    Gt  e1 e2    -> do {compile e1; raw " > "; compile e2}
    Geq e1 e2    -> do {compile e1; raw " >= "; compile e2}

    In e1 e2     -> do {compile e2; raw " as "; compile e1}
    Is e p       -> do {raw "_("; compile e; raw ", "; compile p; raw ")"}
    Subset e1 e2 -> do {compile e1; raw " subset "; compile e2}

    Assign e1 e2 -> do {compile e1; raw " = "; compile e2}

    Bars e       -> do {raw $ "informl.size("; compile e; raw ")"}

    Bracks (Tuple es) -> do {raw "array ("; compileIntersperse ", " es; raw ")"}
    Bracks e -> do {raw "array ("; compile e; raw ")"}
    Braces (Tuple es) -> do {raw "array ("; compileIntersperse ", " es; raw ")"}
    Braces e -> do {raw "array ("; compile e; raw ")"}
    ListItem c e -> do {compile c; raw "["; compile e; raw "]"}
    DictItem k v -> do {compile k; raw "=>"; compile v}

    FunApp v es  -> 
      do string (let v' = if v!!0 == '$' then tail v else v in replace "$" "." v')
         do {raw "("; compileIntersperse ", " es; raw ")"}

    IndexItem e es ->
      do {compile e; raw "["; compileIntersperse "][" es; raw "]"}

    Lambda vs e -> 
      do raw $ "function($" ++ (join ",$" vs) ++ "){return "
         compile e
         raw "}"
    
    _ -> do string "NULL"
    
instance ToPHP Pattern where
  compile p = case p of
    PatternVar v    -> do raw $ "$" ++ v
    PatternCon c ps ->
      do qualifier <- maybeQualified c
         qualEnv <- getQualEnv
         raw $ maybe ("$" ++ c) (\x -> "$" ++ x ++ "->" ++ c) qualifier ++ "(" ++ (underscores ps)
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
    Otherwise ss ->
      do raw "->end;}catch(err){"
         mapM compile ss
         raw "}"

compilePatternVars :: [Pattern] -> Compilation ()
compilePatternVars xs = case xs of
  [] -> do nothing
  [PatternVar v] -> do raw $ "$" ++ v
  (PatternVar v):ps  -> do {raw $ "$" ++ v ++ ","; compilePatternVars ps}
  [PatternCon c ps] -> do compilePatternVars ps
  (PatternCon c ps):ys -> do {compilePatternVars ps; raw ","; compilePatternVars ys}

compileIntersperse :: ToPHP a => String -> [a] -> Compilation ()
compileIntersperse s xs = case xs of
  []   -> do nothing
  [x]  -> do compile x
  x:xs -> do {compile x; raw s; compileIntersperse s xs }

underscores :: [a] -> String
underscores [] = ""
underscores [x] = "_"
underscores (x:xs) = "_," ++ underscores xs

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
defineUnqual ps = if ps == []  then "" else "\\uxadt\\_({" ++ defAux ps ++ "});\n"

defineQual :: QualEnv -> String
defineQual []          = ""
defineQual ((q,ps):qs) = if ps == [] then defineQual qs 
                         else "\\uxadt\\qualified(\'" ++ q ++ "\', {" ++ defAux ps ++ "});\n"
                              ++ defineQual qs

defAux :: [(String,Int)] -> String
defAux [] = ""
defAux [(s,i)] = "\'" ++ s ++ "\'=>array(" ++ defUnderscores i ++ ")"
defAux ((s,i):xs) = "\'" ++ s ++ "\'=>array(" ++ defUnderscores i ++ ")," ++ (defAux xs)

defUnderscores :: Int -> String
defUnderscores 0 = ""
defUnderscores 1 = "_"
defUnderscores i = "_," ++ defUnderscores (i-1)


                                        
                                        

--eof
