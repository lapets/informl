----------------------------------------------------------------
--
-- Informl
-- Meta-language for quickly defining DSLs, including their
-- abstract syntax data structures, parsers, prettyprinters,
-- and other transformations.
--
-- Language/Informl/Compilation/JavaScript.hs
--   Haskell implementation of the Informl language JavaScript
--   compilation algorithm.
--

----------------------------------------------------------------
-- 

module Language.Informl.Compilation.JavaScript
  where

import Data.String.Utils (join)

import Language.Informl.Compilation
import Language.Informl.AbstractSyntax

----------------------------------------------------------------
-- 

class ToJavaScript a where
  compile :: a -> Compilation ()

instance ToJavaScript Top where
  compile (Top m) = compile m

instance ToJavaScript Module where
  compile (Module m ss) = 
    do raw $ "var " ++ m ++ " = (function(uxadt, Informl){"
       setModule m
       indent
       newline
       raw $ "var " ++ m ++ " = {};"
       newline
       mapM compile ss
       newline
       newline
       raw $ "return " ++ m ++ ";"
       unindent
       newline
       raw "}(uxadt, Informl));"

instance ToJavaScript StmtLine where
  compile (StmtLine s) = do {newline; compile s}

instance ToJavaScript Block where
  compile b = case b of
    (Stmt s) -> do compile s
    (Block ss) ->
      do indent
         mapM compile ss
         unindent
         newline

instance ToJavaScript Stmt where
  compile s = case s of

    For (In e1 e2) b ->
      do nothing
      
    If (Is e p) b ->
      do tmp <- freshWithPrefix "__iml"
         decs <- return $ compilePatternVars tmp p
         raw $ "if (" ++ tmp ++ " = "
         compile (Is e p)
         raw ") {"
         raw $ join "" decs
         compile b
         raw "}"

    Function f xs b ->
      do m <- getModule
         d <- depth
         pre <- return $
           if d == 0 then 
             maybe "var " (\m -> m++".") m
           else
             "var "
         newline
         raw $ pre ++ f ++ " = function (" ++ join ", " xs ++ ") {"
         nest
         compile b
         unnest
         string "}"

    For e b ->
      do raw $ "for "
         compile e
         raw " {"
         compile b
         raw "}"

    While e b -> do {raw "while "; compile e; raw " {"; compile b; raw "}"}
    If e b -> do {raw $ "if ("; compile e; raw ") {"; compile b; raw "}"}
    ElseIf e b -> do {raw $ "else if ("; compile e; raw ") {"; compile b; raw "}"}
    Else b -> do {raw $ "else {"; compile b; raw "}"}
    -- Global e -> do {string "global "; compile e; string ";"}
    Local e -> do {string "var "; compile e; string ";"}
    Return e -> do {string "return "; compile e; string ";"}
    Continue -> do string "continue;"
    Break -> do string "break;"
    StmtExp e -> do { compile e; string ";" }

instance ToJavaScript Exp where
  compile e = case e of
    Var v        -> do string v
    Int n        -> do string $ show n
    ConApp c es  ->
      do raw $ "uxadt.C(\"" ++ c ++ "\", "
         raw "["
         compileIntersperse ", " es
         raw "])"

    Concat e1 e2   -> do {compile e1; raw " + "; compile e2}

    Plus e1 e2   -> do {compile e1; raw " + "; compile e2}
    Minus e1 e2  -> do {compile e1; raw " - "; compile e2}

    In e1 e2     -> do {compile e1; raw " in "; compile e2}
    Is e p       -> do {raw $ "uxadt.M("; compile e; raw ", "; compile p; raw ")"}
    Subset e1 e2 -> do {compile e1; raw " subset "; compile e2}

    Assign e1 e2 -> do {compile e1; raw " = "; compile e2}

    Bars e       -> do {raw $ "Informl.size("; compile e; raw ")"}

    _ -> do string "null"
    
instance ToJavaScript Pattern where
  compile p = case p of
    PatternVar v    -> do raw $ "uxadt.V(\"" ++ v ++ "\")"
    PatternCon c ps ->
      do raw $ "uxadt.C(\"" ++ c ++ "\", "
         raw "["
         compileIntersperse ", " ps
         raw "])"

compilePatternVars :: String -> Pattern -> [String]
compilePatternVars tmp p = case p of
  PatternVar v    -> ["var " ++ v ++ " = " ++ tmp ++ "[\"" ++ v ++ "\"];"]
  PatternCon c ps -> concat $ map (compilePatternVars tmp) ps

compileIntersperse :: ToJavaScript a => String -> [a] -> Compilation ()
compileIntersperse s xs = case xs of
  []   -> do nothing
  [x]  -> do compile x
  x:xs -> do {compile x; raw s; compileIntersperse s xs }

--eof
