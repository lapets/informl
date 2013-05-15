----------------------------------------------------------------
--
-- Informl
-- Meta-language for quickly defining DSLs, including their
-- abstract syntax data structures, parsers, prettyprinters,
-- and other transformations.
--
-- Language/Informl/AbstractSyntax.hs
--   Haskell implementation of the Informl language abstract
--   syntax data structure.
--

----------------------------------------------------------------
--

module Language.Informl.AbstractSyntax 
  where

import Data.String.Utils (join)

----------------------------------------------------------------
-- Abstract syntax.

type Variable = String

data Top =
    Top Module
  deriving Eq

data Module =
    Module String [StmtLine]
  deriving Eq

data StmtLine =
    StmtLine Stmt
  deriving Eq

data Stmt =
    Function Variable [Variable] Block
  | For Exp Block
  | While Exp Block
  | If Exp Block
  | ElseIf Exp Block
  | Else Block
  | Global Exp
  | Local Exp
  | Return Exp
  | Continue
  | Break
  | StmtExp Exp
  deriving Eq

data Block = 
    Stmt Stmt
  | Block [StmtLine]
  deriving Eq

data Exp =
    Var String
  | Int Int
  | CNothing
  | CTrue
  | CFalse
  
  | ConApp String [Exp]
  
  | IfExp Exp Exp Exp

  | Comma Exp Exp
  | Ldots Exp Exp

  | Mapsto Exp Exp

  | And Exp Exp
  | Or Exp Exp

  | Is Exp Pattern
  | In Exp Exp
  | Subset Exp Exp
  
  | Assign Exp Exp

  | Eq Exp Exp
  | Neq Exp Exp
  | Lt Exp Exp
  | Gt Exp Exp
  | Leq Exp Exp
  | Geq Exp Exp
  
  | Union Exp Exp
  | Intersect Exp Exp
  
  | Max Exp
  | Min Exp
  
  | Domain Exp
  | Codomain Exp

  | Concat Exp Exp

  | Plus Exp Exp
  | Minus Exp Exp
  
  | Mult Exp Exp
  | Div Exp Exp

  | Pow Exp Exp
  
  | Not Exp
  | Neg Exp
  
  | Dot Exp Exp
  
  | Parens Exp
  | BracksEmpty
  | Bracks Exp
  | BracesEmpty
  | Braces Exp
  | Bars Exp

  | Phrase [PhraseAtom]

  | FunApp Variable [Exp]
  | Tuple [Exp]
  deriving Eq

data PhraseAtom =
    PhraseExp Exp
  | PhraseWord String
  deriving Eq

data Pattern =
    PatternVar String
  | PatternCon String [Pattern]
  deriving Eq

----------------------------------------------------------------
-- Helper functions useful to the parser.

tuple :: [Exp] -> Exp
tuple [e] = e
tuple es = Tuple es

----------------------------------------------------------------
-- Printing functions for the abstract syntax.

showTuple [] = "()"
showTuple [x] = x
showTuple xs = "("++foldr (\s-> \t-> s++", "++t) 
                          (last xs) (init xs)++")"
instance Show Top where
  show (Top m) = show m

instance Show Module where
  show (Module m ls) = "\nmodule " ++ m ++ "\n  " ++ (foldr (\x y->showStmtLine "  " x++"\n\n  "++y) (show $ last ls) (init ls)) ++ "\n\n"

instance Show StmtLine where
  show (StmtLine s) = show s

instance Show Stmt where
  show s = showStmt "" s

instance Show Block where
  show b = case b of
    Block ls -> join "\n" [show l | l <- ls]
    Stmt s -> show s

instance Show Exp where
  show e = showExp e

foldToLines indent = (\l -> foldr (\x y->x++"\n"++indent++y) (last l) (init l) )

showStmtLine indent s = case s of
  StmtLine s -> showStmt indent s

showStmt indent s = case s of
  Function f xs b -> indent ++ "function" ++ " " ++ f ++ " (" ++ join ", " xs ++ ")" ++ "\n" ++ showBlock (indent++"  ") b
  For e b         -> indent ++ "for" ++ " " ++ showExp e ++ "\n" ++ showBlock (indent++"  ") b
  While e b       -> indent ++ "while" ++ " " ++ showExp e ++ "\n" ++ showBlock (indent++"  ") b
  If e b          -> indent ++ "if" ++ " " ++ showExp e ++ "\n" ++ showBlock (indent++"  ") b
  ElseIf e b      -> indent ++ "elseif" ++ " " ++ showExp e ++ "\n" ++ showBlock (indent++"  ") b
  Else b          -> indent ++ "else" ++ "\n" ++ showBlock (indent++"  ") b
  Global e        -> indent ++ "global" ++ " " ++ showExp e
  Local e         -> indent ++ "local" ++ " " ++ showExp e
  Return e        -> indent ++ "return" ++ " " ++ showExp e
  Continue        -> indent ++ "continue"
  Break           -> indent ++ "break"
  StmtExp e       -> indent ++ showExp e

showBlock indent b = case b of
  Block ls -> join "\n" $ map (showStmtLine indent) ls
  Stmt s   -> showStmt indent s

showExp e = case e of
  Int n -> show n
  Var x -> x
  
  ConApp s es -> if length es == 0 then s else s ++ "(" ++ (join ", " (map showExp es)) ++ ")"
  
  Comma e1 e2 -> showExp e1 ++ " in " ++ showExp e2
  Ldots e1 e2 -> showExp e1 ++ " in " ++ showExp e2
  Mapsto e1 e2 -> showExp e1 ++ " in " ++ showExp e2
  And e1 e2 -> showExp e1 ++ " in " ++ showExp e2
  Or e1 e2 -> showExp e1 ++ " in " ++ showExp e2
  Is e p -> showExp e ++ " in " ++ showPattern p
  In e1 e2 -> showExp e1 ++ " in " ++ showExp e2
  Subset e1 e2 -> showExp e1 ++ " in " ++ showExp e2
  Assign e1 e2 -> showExp e1 ++ " in " ++ showExp e2
  Eq e1 e2 -> showExp e1 ++ " in " ++ showExp e2
  Neq e1 e2 -> showExp e1 ++ " in " ++ showExp e2
  Lt e1 e2 -> showExp e1 ++ " in " ++ showExp e2
  Gt e1 e2 -> showExp e1 ++ " in " ++ showExp e2
  Leq e1 e2 -> showExp e1 ++ " in " ++ showExp e2
  Geq e1 e2 -> showExp e1 ++ " in " ++ showExp e2
  Union e1 e2 -> showExp e1 ++ " in " ++ showExp e2
  Intersect e1 e2 -> showExp e1 ++ " in " ++ showExp e2
  Max e -> "max " ++ showExp e
  Min e -> "min " ++ showExp e
  Domain e -> "domain " ++ showExp e
  Codomain e -> "codomain " ++ showExp e
  Plus e1 e2 -> showExp e1 ++ " + " ++ showExp e2
  Minus e1 e2 -> showExp e1 ++ " + " ++ showExp e2
  Mult e1 e2 -> showExp e1 ++ " + " ++ showExp e2
  Div e1 e2 -> showExp e1 ++ " + " ++ showExp e2
  Pow e1 e2 -> showExp e1 ++ " + " ++ showExp e2
  Not e -> "not " ++ showExp e
  Neg e -> "-" ++ showExp e
  Dot e1 e2 -> showExp e1 ++ "." ++ showExp e2
  Parens e -> "(" ++ showExp e ++ ")"
  Bracks e -> "[" ++ showExp e ++ "]"
  Braces e -> "{" ++ showExp e ++ "}"
  Bars e -> "|" ++ showExp e ++ "|"
  BracksEmpty -> "[]"
  BracesEmpty -> "{}"

  Tuple es -> showTuple (map (showExp) es)

showPattern p = case p of
  PatternVar x    -> x
  PatternCon c ps -> if length ps == 0 then c else c ++ "(" ++ (join ", " (map showPattern ps)) ++ ")"

--eof
