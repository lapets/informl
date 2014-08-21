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
import Data.List.Utils (replace)

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
    do setModule m
       expandNameSpace [(m, functionDecs ss)]
       unqual <- setQualEnv ss
       newline
       raw $ "(function(uxadt, Informl){"
       indent
       raw "\"use strict\"; var uxadt, Informl;"
       newline
       raw $ "var " ++ m ++ " = {};"
       newline
       raw "if(typeof exports !== 'undefined') {"
       raw "if (typeof module !== 'undefined' && module.exports){"
       raw $ "exports = module.exports = "++ m ++";}" 
       raw $ "exports." ++ m ++ " = " ++ m ++ ";"
       raw "uxadt = require('./uxadt.js');"
       raw "Informl = require('./Informl.js');}"
       newline
       raw $ "else {window." ++ m ++ " = " ++ m ++ ";"
       raw $ "uxadt = window.uxadt; Informl = window.Informl;}"
       newline
       raw $ defineQual $ getQualifiedNow ss
       newline
       raw $ defineUnqual unqual
       newline
       mapM compile ss
       newline
       unindent
       raw $ "}(typeof exports !== 'undefined' ? exports : (this." ++ m ++" = {})));"

instance ToJavaScript StmtLine where
  compile (StmtLine s) = do {newline; compile s}

instance ToJavaScript Block where
  compile b = case b of
    Stmt (If (Is e (PatternCon c ps)) b ) ->
      do tmp <- freshWithPrefix "__iml"
         qualifier <- maybeQualified c 
         qualEnv <-getQualEnv
         raw $ "var " ++ tmp ++ "="
         compile e
         raw ";"
         newline
         raw $ "return " ++ tmp ++ "."
         raw $ "_(" ++ (maybe c (\x -> (x ++ "." ++ c)) qualifier)  ++ "(" ++ underscores ps qualEnv ++ ")"
         raw ", function("
         compilePatternVars ps 
         raw ") {"
         compile b
         raw "})->end();"
         newline
    Block (StmtLine (If (Is e (PatternCon c ps)) b ) : ss) ->
      do tmp <- freshWithPrefix "__iml"
         qualifier <- maybeQualified c
         qualEnv <-getQualEnv
         raw $ "var " ++ tmp ++ "="
         compile e
         raw ";"
         newline
         raw $ "return " ++ tmp ++ "."
         newline
         raw $ "_(" ++ (maybe c (\x -> x ++ "." ++ c) qualifier)  ++ "(" ++ underscores ps qualEnv ++ ")"
         raw ", function(" 
         compilePatternVars ps 
         raw ") {"
         compile b
         raw "})"
         newline
         raw "."
         nestPatterns ss
         newline 
    Stmt s -> do compile s
    Block ss ->
      do indent
         mapM compile ss
         unindent
         newline

instance ToJavaScript Stmt where
  compile s = case s of

    For (In e1 e2) b ->
      do obj <- freshWithPrefix "__iml"
         i <- freshWithPrefix "__iml"
         bool <- freshWithPrefix "__iml"
         raw $ "var " ++ bool ++ " = false;" ++ "var " ++ obj ++ " = "
         compile e2
         raw $ "; if(Informl.type(" ++ obj ++ ")==='pattern') { " ++ obj ++ " = Informl.unpack(" ++ obj ++ "); "
                  ++ bool ++" = true;}"
         newline
         raw $ " if(Informl.type(" ++ obj ++ ")==='list') { "++ obj ++ " = Informl.listToDict(" ++ obj ++ "); "
                ++ bool ++" = true;}"
         raw $ "for (var " ++ i ++ " in " ++ obj ++ "){ var "
         compile e1
         raw $ " = " ++ bool ++ " ? " ++ obj ++ "[" ++ i ++ "] : " ++ i ++ ";"
         compile b
         raw "}"
      
    If (Is e (PatternCon c ps)) b  ->
      do tmp <- freshWithPrefix "__iml"
         qualifier <- maybeQualified c
         qualEnv <- getQualEnv
         raw $ "var " ++ tmp ++ "="
         compile e
         raw ";"
         newline
         raw $ "return " ++ tmp ++ "."
         raw $ "_(" ++ (maybe c (\x -> x ++ "." ++ c) qualifier)  ++ "(" ++ underscores ps qualEnv ++ ")"
         raw ", function("
         compilePatternVars ps 
         raw ") {"
         compile b
         raw "}).end();"
         newline

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

    Set v e wb ->
      do tmp <- freshWithPrefix "__iml"
         err <- return $ "throw new Error('Pattern did not match');"
         othw <- return $ hasOtherwise wb
         raw $ "var "++ tmp ++ " = ("
         compile e
         raw ");"
         raw $ "var "++ v ++ " = null; " ++ v ++ " = " ++ tmp
         compilePatCheck wb
         raw ".end;"
         newline
         raw $ "if(" ++ v ++ " == null){"
         raw $ if othw == [] then err else ""
         compileOtherwiseBlock v othw
         raw "}"
         newline
         raw $ "else " ++ v ++ " = " ++ tmp
         mapM compile wb
         raw ".end;"

    Get e wb ->
      do eval <- freshWithPrefix "__iml"
         tmp <- freshWithPrefix "__iml"
         err <- return $ "throw new Error('Pattern did not match');"
         othw <- return $ hasOtherwise wb
         raw $ "var " ++ eval ++ " = "
         compile e
         raw ";"
         raw $ "var "++ tmp ++ " = null; " ++ tmp ++ " = " ++ eval
         compilePatCheck wb
         raw ".end;"
         raw $ "if(" ++ tmp ++ " == null){"
         raw $ if othw == [] then err else ""
         mapM compile othw
         raw "}"
         newline
         raw $ tmp ++ " = " ++ eval
         mapM compile wb
         raw ".end;"
         newline
         raw $"return " ++ tmp ++ ";"

    For e b -> do {raw "for "; raw "("; compile e; raw ")"; raw " {"; compile b; raw "}"}
    While e b -> do {raw "while "; raw "("; compile e; raw ") {"; compile b; raw "}"}
    If e b -> do {raw $ "if ("; compile e; raw ") {"; compile b; raw "}"}
    ElseIf e b -> do {raw $ "else if ("; compile e; raw ") {"; compile b; raw "}"}
    Else b -> do {raw $ "else {"; compile b; raw "}"}
    Global e -> do {compile e; string ";"}
    Local e -> do {string "var "; compile e; string ";"}
    Return e -> do {string "return "; compile e; string ";"}
    Value e -> do {string "return "; compile e; string ";"}
    Continue -> do string "continue;"
    Break -> do string "break;"
    StmtExp e -> do { compile e; string ";" }
    Where _ -> do nothing

instance ToJavaScript Exp where
  compile e = case e of
    Var v -> 
      do ns <- getNameSpace 
         string $ maybe v (\x-> x ++ "." ++ v) $ maybeNamed v ns
    CTrue        -> do string "true"
    CFalse       -> do string "false"
    CNothing     -> do string "null"
    Int n        -> do string $ show n
    Float n      -> do string $ show n
    Literal s    -> do string $ "\"" ++ (compileLiteral s) ++ "\""
    ConApp c es  ->
                do qual <- maybeQualified c
                   raw $ (maybe c (\x-> x ++ "." ++c) qual) ++ "("
                   compileIntersperse ", " es
                   raw ")"

    Concat e1 e2 -> do {compile e1; raw " + "; compile e2}
    Neg (Int i)  -> do {raw $ "-" ++ (show i);}
    Neg (Float f)-> do {raw $ "-" ++ (show f);}
    Pow   e1 e2  -> do {raw "("; compile e1; raw "^"; compile e2; raw ")"}
    Mult  e1 e2  -> do {raw "("; compile e1; raw " * "; compile e2; raw ")"}
    Div   e1 e2  -> do {raw "("; compile e1; raw " / "; compile e2; raw ")"}
    Plus  e1 e2  -> do {raw "Informl.plus("; compile e1; raw ", "; compile e2; raw ")"}
    Minus e1 e2  -> do {raw "Informl.minus("; compile e1; raw ", "; compile e2; raw ")"}
    Mod   e1 e2  -> do {raw "("; compile e1; raw " % "; compile e2; raw ")"}

    Eq  e1 e2    -> do {compile e1; raw " == "; compile e2}
    Neq e1 e2    -> do {compile e1; raw " != "; compile e2}
    Lt  e1 e2    -> do {compile e1; raw " < "; compile e2}
    Leq e1 e2    -> do {compile e1; raw " <= "; compile e2}
    Gt  e1 e2    -> do {compile e1; raw " > "; compile e2}
    Geq e1 e2    -> do {compile e1; raw " >= "; compile e2}

    And e1 e2    -> do {raw "("; compile e1; raw " && "; compile e2; raw ")"}
    Or  e1 e2    -> do {raw "("; compile e1; raw " || "; compile e2; raw ")"}
    Not e        -> do {raw "(!("; compile e; raw "))"}
    
    In e1 e2     -> do {compile e1; raw " in "; compile e2}
    Is e p       -> do {raw "uxadt.M("; compile e; raw ", "; compile p; raw ")"}
    Subset e1 e2 -> do {compile e1; raw " subset "; compile e2}

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
      do raw "Informl.slice("
         compile c
         raw ", "
         compile d
         raw ", null)"
    IndexItem c [(DictItem (Var "_") d)] ->
      do raw "Informl.slice("
         compile c
         raw ", null,"
         compile d
         raw ")"
    IndexItem c [(DictItem e f)] ->
      do raw "Informl.slice("
         compile c
         raw ", "
         compile e
         raw ", "
         compile f
         raw ")"
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
      do raw $ "function(" ++ (join "," ss) ++ "){return "
         compile e
         raw "}"

    Dot (ConApp c []) f -> do {raw c; raw "."; compile f}
    Dot e f -> do {compile e; raw "."; compile f}

    IfExp t e f ->
      do raw "("
         compile e
         raw " ? "
         compile t
         raw " : "
         compile f
         raw ")"
    
    _ -> do string "null"
    
instance ToJavaScript Pattern where
  compile p = case p of
    PatternVar v    -> do raw v
    PatternCon c ps ->
      do qualifier <- maybeQualified c
         qualEnv <- getQualEnv
         raw $ maybe c (\x -> x ++ "." ++ c) qualifier ++ "(" ++ underscores ps qualEnv
         raw ")"
--    AnonPattern ps ->
--      do qualEnv <- getQualEnv
--         raw $ "anonymous.anonymous_" ++ (show (length ps)) ++ "(" ++ underscores ps qualEnv ++ ")"

instance ToJavaScript WhenBlock where
  compile wb = case wb of 
    WhenBlock p ss -> 
      do raw "._("
         compile p
         raw ", function ("
         compilePatternVars [p]
         raw ") {"
         mapM compile ss
         raw "} )"
    Otherwise ss ->
      do nothing


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

compilePatCheck :: [WhenBlock] -> Compilation ()
compilePatCheck wb = case wb of
  [] -> do nothing
  ((WhenBlock p b):rest) ->
      do raw "._("
         compile p
         raw ", function(){return true;})"
         compilePatCheck rest
  [Otherwise _ ] -> do nothing

compileOtherwiseBlock :: String -> [StmtLine] ->Compilation ()
compileOtherwiseBlock v ss = case ss of
  ((StmtLine (Value e)):ss) -> 
                  do raw $ v ++ " = "
                     compile e
                     raw ";"
  (s:ss) -> do compile s
               compileOtherwiseBlock v ss
  [] -> do nothing

  

compileIntersperse :: ToJavaScript a => String -> [a] -> Compilation ()
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
underscores [PatternVar v] _ = "null"
underscores ((PatternVar v):xs) qe = "null," ++ underscores xs qe

nestPatterns :: [StmtLine] -> Compilation ()
nestPatterns ss = case ss of
  [] -> do {raw "end;"; newline} 
  StmtLine (If (Is e (PatternCon c ps)) b):ss -> 
    do tmp <- freshWithPrefix "__iml"
       qualEnv <- getQualEnv
       qualifier <- maybeQualified c
       raw $ "_(" ++ (maybe c (\x -> x ++ "." ++ c) qualifier) ++ "(" ++ (underscores ps qualEnv) ++ ")" ++ ", function(" 
       compilePatternVars ps 
       raw ") {"
       compile b
       raw $ "})\n."
       nestPatterns ss
  StmtLine (ElseIf (Is e (PatternCon c ps)) b):ss ->
    do tmp <- freshWithPrefix "__iml"
       qualEnv <- getQualEnv
       qualifier <- maybeQualified c
       raw $ "_(" ++ (maybe c (\x -> x ++ "." ++ c) qualifier) ++ "(" ++ (underscores ps qualEnv) ++ ")" ++ ", function("
       compilePatternVars ps 
       raw ") {"
       compile b
       raw $ "})"
       newline
       raw "."
       nestPatterns ss
  ss -> do {raw "end;"; newline; compile (Block ss)}

compilePatternVars2 :: String -> Pattern -> [String]
compilePatternVars2 tmp p = case p of
  PatternVar v    -> ["var " ++ v ++ " = " ++ tmp ++ "[\"" ++ v ++ "\"];"]
  PatternCon c ps -> concat $ map (compilePatternVars2 tmp) ps

defineUnqual :: [(Constructor,Int)] -> String
defineUnqual ps = if ps == []  then "" else "uxadt._({" ++ defAux ps ++ "});\n"

defineQual :: QualEnv -> String
defineQual []          = ""
defineQual ((q,ps):qs) = if ps == [] then defineQual qs 
                         else "uxadt.qualified(\'" ++ q ++ "\', {" ++ defAux ps ++ "});\n"
                              ++ defineQual qs


defAux :: [(Constructor,Int)] -> String
defAux [] = ""
defAux [(s,i)] = "\'" ++ s ++ "\':[" ++ defUnderscores i ++ "]"
defAux ((s,i):xs) = "\'" ++ s ++ "\':[" ++ defUnderscores i ++ "]," ++ (defAux xs)

defUnderscores :: Int -> String
defUnderscores 0 = ""
defUnderscores 1 = "null"
defUnderscores i = "null," ++ defUnderscores (i-1)

--eof
