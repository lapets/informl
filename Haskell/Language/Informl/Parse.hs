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
topP = do { whiteSpace ; m <- moduleP ; eof ; return $ Top m }

moduleP :: ParseFor Module
moduleP = withIndent (do { res "module" ; m <- con ; return m }) stmtLineP Module

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
      do {c <- con; res "is"; d <- defrP; return $ DefIs c d}
  <|> do {cs <- bracks $ sepBy con commaSep; res "are"; d <- defrP; return $ DefAre cs d}
  <|> do {cs <- bracks $ sepBy con commaSep; res "arent"; d <- defrP; return $ DefArent cs d}
  <|> do {c <- con; res "isnt"; d <- defrP; return $ DefIsnt c d}
  <|> do {res "all"; res "are"; d <- defrP; return $ All d}
  <|> do {res "none"; res "are"; d <- defrP; return $ None d}

defrP :: ParseFor Definer
defrP =
      do {res "qualified"; c <- con; return $ Qualified c}
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
  <|> withIndent (do { res "for" ; e <- expP; return e }) stmtLineP (\e ss -> For e (Block ss))
  <|> withIndent (do { res "while" ; e <- expP; return e }) stmtLineP (\e ss -> While e (Block ss))
  <|> withIndent (do { res "if" ; e <- expP; return e }) stmtLineP (\e ss -> If e (Block ss))
  <|> withIndent (do { res "elseif" ; e <- expP; return e }) stmtLineP (\e ss -> ElseIf e (Block ss))
  <|> withIndent (do { res "else" }) stmtLineP (\_ ss -> Else (Block ss))
  <|> withIndent (do { res "where"}) defLineP (\_ ds -> Where ds)
  <|> withIndent (do {res "set"; v <- var; res "when"; e <- expP; return (v,e)}) whenBlockP (\(v,e) wb -> Set v e wb)
  <|> withIndent (do {res "get"; e <- expP; return e}) whenBlockP (\e wb -> Get e wb)
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
      (do { s <- PT.stringLiteral lang; return $ Literal s})
  <|> try isParser
  <|> atomParser

varP :: ParseFor Exp
varP = do { v <- var; return $ Var v } <?> "variable"

varOrFunAppP :: ParseFor Exp
varOrFunAppP =
  do { v <- var
     ; es <- option [] (parens $ sepBy1 expP commaSep)
     ; return $ if length es == 0 then Var v else FunApp v es
     }

intP :: ParseFor Exp
intP = do { n <- natural; return $ (Int $ fromInteger n) } <?> "integer"

isParser :: ParseFor Exp
isParser = do { e <- atomParser; res "is"; p <- patternP; return $ Is e p }

patternP :: ParseFor Pattern
patternP =
      ( do { c <- con; ps <- option [] (parens $ sepBy1 patternP commaSep); return $ PatternCon c ps } )
  <|> ( do { v <- var; return $ PatternVar v } )

atomParser :: ParseFor Exp
atomParser = PE.buildExpressionParser exprOps nonAppParser

exprOps :: PE.OperatorTable String () ParseState Exp
exprOps =
  [ [ binary "^" Pow PE.AssocLeft
    ]
  , [ binary "*" Mult PE.AssocLeft
    , binary "/" Div PE.AssocLeft 
    ]
  , [ binary "+" Plus PE.AssocLeft
    , binary "-" Minus PE.AssocLeft
    ]
  , [ binary "@" Concat PE.AssocLeft
    , binary "!!" ListItem PE.AssocLeft
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
  , [ binary ":=" Assign PE.AssocLeft
    ]
  ]

nonAppParser :: ParseFor Exp
nonAppParser =
      varOrFunAppP
  <|> intP
  <|> conAppP
  <|> parens tupleParser
  <|> bracksP
  <|> barsP
  <|> (do{res "true"; return CTrue})
  <|> (do{res "false"; return CFalse})
  <|> (do{res "nothing"; return CNothing})

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

----------------------------------------------------------------
-- Parsec-specific configuration definitions and synonyms.

langDef :: PL.GenLanguageDef String () ParseState
langDef = PL.javaStyle
  { PL.identStart        = oneOf "abcdefghijkmlnopqrstuvwxyz_$" -- Only lowercase.
  , PL.identLetter       = alphaNum <|> oneOf "_'$"
  , PL.opStart           = PL.opLetter langDef
  , PL.opLetter          = oneOf "+*=&|:\\[]()"
  , PL.reservedOpNames   = [ "@" 
                           , "+", "-", "*", "/", "^"
                           , "=", "<", ">", "<=", ">=", "!="
                           , "is", "in", "subset", "union", "intersect", "and", "or"
                           , ".", ",", "...", ":="
                           ]
  , PL.reservedNames     = [ "module" , "where", "when", "set", "get"
                           , "function","for","while","if", "else", "elseif"
                           , "global","local","return","continue","break"
                           , "domain","codomain","true","false","nothing"
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
natural    = PT.natural lang
parens p   = between (symbol "(") (symbol ")") p
bracks p   = between (symbol "[") (symbol "]") p
bars   p   = between (symbol "|") (symbol "|") p
commaSep   = skipMany1 (symbol ",")

binary name f assoc = PE.Infix (do{PT.reservedOp lang name; return f}) assoc
prefix name f       = PE.Prefix (do{PT.reservedOp lang name; return f})

withIndent p1 p2 f = PI.withBlock f p1 p2

con = do { c <- oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ; cs <- option "" var ; return $ c:cs }

--eof
