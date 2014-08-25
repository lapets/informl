----------------------------------------------------------------
-- Parser definition.
--

----------------------------------------------------------------
-- Parser to convert concrete syntax to abstract syntax.

module Language.Informl.Parse (parseString) where

import Text.Parsec
import qualified Text.Parsec.Indent as PI (withBlock, runIndent)
import qualified Text.Parsec.Token as PT
import qualified Text.Parsec.Expr as PE
import qualified Text.ParserCombinators.Parsec.Language as PL
import qualified Text.ParserCombinators.Parsec.Prim as Prim

import Control.Monad.Trans.State.Lazy (StateT)
import Data.Functor.Identity (Identity)

import Language.Informl.AbstractSyntax


----------------------------------------------------------------
-- Exported functions.

parseString :: String -> Either ParseError Top
parseString s = PI.runIndent "" $ runParserT topP () "" s

----------------------------------------------------------------
-- Top-level parser.

type ParseState = StateT SourcePos Identity
type ParseFor a = ParsecT [Char] () ParseState a

topP :: ParseFor Top
topP = do { whiteSpace ; m <- modP ; eof ; return $ Top m }

modP :: ParseFor Module
modP = withIndent (do { res "module" ; m <- con ; return m }) stmtLineP Module

stmtLineP :: ParseFor StmtLine
stmtLineP = do { s <- stmtP ; return $ StmtLine s }

defLineP :: ParseFor DefLine
defLineP = do { d <- defP ; return $ DefLine d }

blockP :: ParseFor Block
blockP =
      do { s <- stmtP ; return $ Stmt s }
  <|> do { ls <- many1 stmtLineP ; return $ Block ls }

----------------------------------------------------------------
-- Definition Parser

defP :: ParseFor Definition
defP =
      do {res "all are"; d <- defrP; return $ All d}
  <|> do {res "none are"; d <- defrP; return $ None d}
  <|> do {res "all were"; d <- defrP; return $ AllB4 d}
  <|> do {c <- defCon; defOne c}
  <|> do {cs <- bracks $ sepBy defCon commaSep; defMult cs}

defOne :: String -> ParseFor Definition
defOne c = 
        do {res "is"; d <- defrP; return $ DefIs c d}
    <|> do {res "was"; d <- defrP; return $ DefWas c d}
    <|> do {res "isnt"; d <- defrP; return $ DefIsnt c d}

eitherVarOrCon =
        do {x <- var; return x}
    <|> do {x <- con; return x}

defMult :: [String] -> ParseFor Definition
defMult cs =
        do {res "are"; d <- defrP; return $ DefAre cs d}
    <|> do {res "were"; d <- defrP; return $ DefWere cs d}
    <|> do {res "arent"; d <- defrP; return $ DefArent cs d}

defCon = do {c <- con; ps <- option [] (parens $ sepBy1 patternP commaSep); return c}

defrP :: ParseFor Definer
defrP =
      do {res "qualified"; c <- defCon; return $ Qualified c}
  <|> do {res "unqualified"; return Unqualified}
  <|> do {res "associative"; return Associative}
  <|> do {res "commutative"; return Commutative}
  <|> do {res "satisfying"; e <- expP; return $ Satisfying e}

----------------------------------------------------------------
-- When Block Parser

whenBlockP :: ParseFor WhenBlock

whenBlockP = 
      withIndent (do {res "otherwise"}) stmtLineP (\_ b -> Otherwise b)
  <|> withIndent (do {p <- patternP; return p}) stmtLineP (\p b -> WhenBlock p b)
  


----------------------------------------------------------------
-- Statement parser.

stmtP :: ParseFor Stmt
stmtP =
      withIndent 
        (do { res "function" ; f <- var ; xs <- parens $ sepBy var commaSep ; return (f, xs) }) stmtLineP (\(f, xs) ss -> Function f xs (Block ss))
  <|> withIndent 
        (do { res "private" ; f <- var ; xs <- parens $ sepBy var commaSep ; return (f, xs) }) stmtLineP (\(f, xs) ss -> Private f xs (Block ss))
  <|> withIndent (do { res "for" ; e <- expP; return e }) stmtLineP (\e ss -> For e (Block ss))
  <|> withIndent (do { res "while" ; e <- expP; return e }) stmtLineP (\e ss -> While e (Block ss))
  <|> withIndent (do { res "if" ; e <- expP; return e }) stmtLineP (\e ss -> If e (Block ss))
  <|> withIndent (do { res "elseif" ; e <- expP; return e }) stmtLineP (\e ss -> ElseIf e (Block ss))
  <|> withIndent (do { res "else" }) stmtLineP (\_ ss -> Else (Block ss))
  <|> withIndent (do { res "where"}) defLineP (\_ ds -> Where ds)
  <|> withIndent (do {res "set"; v <- var; res "when"; e <- expP; return (v,e)}) whenBlockP (\(v,e) wb -> Set v e wb)
  <|> withIndent (do {res "get"; e <- expP; return e}) whenBlockP (\e wb -> Get e wb)
  <|> do { res "import"; m <- eitherVarOrCon; res "as"; a <- eitherVarOrCon; return (Import m a)}
  <|> do { res "global" ; e <- expP ; return $ Global e }
  <|> do { res "local" ; e <- expP ; return $ Local e }
  <|> do { res "return" ; e <- expP ; return $ Return e }
  <|> do { res "value" ; e <- expP ; return $ Value e }
  <|> do { res "break" ; return Break }
  <|> do { res "continue" ; return Continue }
  <|> do { rO "|" ; e <- expP ; return $ Return e }
  <|> do { e <- expP ; return $ StmtExp e }

----------------------------------------------------------------
-- Expression parser.

expP :: ParseFor Exp
expP = 
      try isParser
  <|> try ifExpP
  <|> atomParser

varP :: ParseFor Exp
varP = do { v <- var; return $ Var v } <?> "variable"

varOrFunAppP :: ParseFor Exp
varOrFunAppP =
      do v <- var
         es <- option [] (parens $ sepBy1 expP commaSep)
         bs <- option [] (bracks $ sepBy1 expP commaSep)
         return $ if length es == 0 && length bs == 0 then Var v
                  else if length bs == 0 && length es > 0 then FunApp v es
                  else if length es == 0 && length bs > 0 then IndexItem (Var v) bs
                  else IndexItem (FunApp v es) bs
     

intP :: ParseFor Exp
intP = do { n <- integer; return $ (Int $ fromInteger n) } <?> "integer"

floatP :: ParseFor Exp
floatP = do { n <- float; return $ (Float $ (read n)) }

numP :: ParseFor Exp
numP =  do {n <- naturalOrFloat; return $ either (\i-> Int (fromInteger i)) (\d-> Float d) n }

isParser :: ParseFor Exp
isParser = do { e <- atomParser; res "is"; p <- patternP; return $ Is e p }

patternP :: ParseFor Pattern
patternP =
      ( do { c <- con; ps <- option [] (parens $ sepBy1 patternP commaSep); return $ PatternCon c ps } )
  <|> ( do { v <- var; return $ PatternVar v } )
  <|> ( do { res "@"; ps <- option [] (parens $ sepBy1 patternP commaSep); return $ AnonPattern ps})

atomParser :: ParseFor Exp
atomParser = PE.buildExpressionParser exprOps nonAppParser

exprOps :: PE.OperatorTable String () ParseState Exp
exprOps =
  [ [ prefix "-" Neg
    , prefix "not" Not 
    ]
  , [ binary "." Dot PE.AssocLeft
    ]
  , [ binary "^" Pow PE.AssocLeft
    ]
  , [ binary "*" Mult PE.AssocLeft
    , binary "/" Div PE.AssocLeft
    , binary "%" Mod PE.AssocLeft
    ]
  , [ binary "+" Plus PE.AssocLeft
    , binary "-" Minus PE.AssocLeft
    ]
  , [ binary "!!" ListItem PE.AssocLeft
    ]
  , [ binary "==" Eq PE.AssocLeft 
    , binary "!=" Neq PE.AssocLeft
    , binary "<" Lt PE.AssocLeft
    , binary ">" Gt PE.AssocLeft
    , binary "<=" Leq PE.AssocLeft
    , binary ">=" Geq PE.AssocLeft
    ]
  , [ binary "in" In PE.AssocLeft
    , binary "subset" Subset PE.AssocLeft
    ]
  , [ binary "or" Or PE.AssocLeft
    , binary "and" And PE.AssocLeft
    ]
  , [ binary "maps" Maps PE.AssocRight
    , binary "folds" Folds PE.AssocRight
    , binary "on" On PE.AssocRight
    ]
  , [ binary ":=" Assign PE.AssocLeft
    , binary "=" Assign PE.AssocLeft
    , binary ":" DictItem PE.AssocLeft
    , binary "+=" PlusAssign PE.AssocLeft
    , binary "-=" MinusAssign PE.AssocLeft
    , binary "*=" MultAssign PE.AssocLeft
    , binary "/=" DivAssign PE.AssocLeft
    ]
  ]

nonAppParser :: ParseFor Exp
nonAppParser =
      varOrFunAppP
  <|> (do { s <- PT.stringLiteral lang; return $ Literal s})
  <|> numP
  <|> conAppP
  <|> parens tupleParser
  <|> bracksP
  <|> bracesP
  <|> barsP
  <|> lambdaP
  <|> (do{res "true"; return CTrue})
  <|> (do{res "false"; return CFalse})
  <|> (do{res "null"; return CNothing})

ifExpP :: ParseFor Exp
ifExpP = do { e <- atomParser; res "if"; f <- atomParser; res "else"; g <- atomParser; return $ IfExp e f g}

lambdaP :: ParseFor Exp
lambdaP = 
  do  ls <- lmbd $ sepBy1 var commaSep
      e <- expP
      return $ Lambda ls e


bracksP :: ParseFor Exp
bracksP =
  do { es <- bracks tupleParser
     ; return $ Bracks es
     }

conAppP :: ParseFor Exp
conAppP = 
  do { c <- con
     ; es <- option [] (parens $ sepBy1 expP commaSep)
     ; return $ ConApp c es
     }

tupleParser = do { es <- sepBy expP commaSep; return $ tuple es } <?> "tuple"

barsP :: ParseFor Exp
barsP =
  do { e <- bars expP
     ; return $ Bars e
     }

bracesP :: ParseFor Exp
bracesP =
  do { e <- braces tupleParser
     ; return $ Braces e
     }

----------------------------------------------------------------
-- Parsec-specific configuration definitions and synonyms.

langDef :: PL.GenLanguageDef String () ParseState
langDef = PL.javaStyle
  { PL.identStart        = oneOf "abcdefghijkmlnopqrstuvwxyz_$"
  , PL.identLetter       = alphaNum <|> oneOf "_'$"
  , PL.opStart           = PL.opLetter langDef
  , PL.opLetter          = oneOf "+*=&|:\\[]()"
  , PL.reservedOpNames   = [ "@" 
                           , "+", "-", "*", "/", "^", "%"
                           , "=", "<", ">", "<=", ">=", "!="
                           , "is", "in", "subset", "union", "intersect", "and", "or", "not"
                           , "maps", "folds", "on"
                           , ".", ",", "...", ":="
                           ]
  , PL.reservedNames     = [ "module" , "where", "when", "set", "get", "otherwise"
                           , "function","private","for","while","if", "else", "elseif"
                           , "global","local","return","continue","break","import","as"
                           , "domain","codomain","true","false","null", "@"
                           ]
  , PL.commentLine       = "#"
  , PL.commentStart      = "/*"
  , PL.commentEnd        = "*/"
  }

lang :: PT.GenTokenParser [Char] () ParseState
lang = PT.makeTokenParser langDef

whiteSpace = PT.whiteSpace lang
symbol     = PT.symbol lang
var        = PT.identifier lang
rO         = PT.reservedOp lang
res        = PT.reserved lang
integer    = PT.integer lang
natural    = PT.natural lang
naturalOrFloat = PT.naturalOrFloat lang
parens p   = between (symbol "(") (symbol ")") p
bracks p   = between (symbol "[") (symbol "]") p
bars   p   = between (symbol "|") (symbol "|") p
braces p   = between (symbol "{") (symbol "}") p
lmbd   p   = between (symbol "\\") (symbol "->") p 
commaSep   = skipMany1 (symbol ",")

binary name f assoc = PE.Infix (do{PT.reservedOp lang name; return f}) assoc
prefix name f       = PE.Prefix (do{PT.reservedOp lang name; return f})

withIndent p1 p2 f = PI.withBlock f p1 p2

con = do { c <- oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ; cs <- option "" var ; return $ c:cs }

float = do {d <- integer; res "."; e <- natural; return $ (show d) ++ "." ++ (show e)}
--eof
