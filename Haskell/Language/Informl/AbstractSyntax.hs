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
  | Private Variable [Variable] Block
  | Where [DefLine]
  | Set Variable Exp [WhenBlock]
  | Get Exp [WhenBlock]
  | For Exp Block
  | While Exp Block
  | If Exp Block
  | ElseIf Exp Block
  | Else Block
  | Global Exp
  | Local Exp
  | Return Exp
  | Value Exp
  | Continue
  | Break
  | StmtExp Exp
  | Throws Exp
  | Import String String
  deriving Eq

data WhenBlock = 
    WhenBlock Pattern [StmtLine]
  | Otherwise [StmtLine]
  deriving Eq

data DefLine =
    DefLine Definition
  deriving Eq

data Definition = 
    DefAre [Variable] Definer 
  | DefIs Variable Definer
  | DefArent [Variable] Definer
  | DefIsnt Variable Definer
  | All Definer
  | None Definer
  | DefWas Variable Definer
  | DefWere [Variable] Definer
  | AllB4 Definer
  deriving Eq

data Definer = 
    Qualified Variable -- to make use of qualified definitions in uxadt
  | Unqualified -- for now redundent
  | Associative -- Equal when dfs is same
  | Commutative -- node is commutative -> node(5,2) == node(2,5)
  | Equaling Pattern -- defines a macro for a pattern
  | Satisfying Exp -- e.g. pricedOver100 is satisfying (price(self) > 100)
  deriving Eq

data Block = 
    Stmt Stmt
  | Block [StmtLine]
  deriving Eq

data Exp =
    Var String
  | Int Int
  | Float Double
  | CNothing
  | CTrue
  | CFalse
  | Literal String
  
  | ConApp String [Exp]
  
  | IfExp Exp Exp Exp

  | Comma Exp Exp
  | Ldots Exp Exp

  | Maps Exp Exp
  | Folds Exp Exp
  | On Exp Exp

  | And Exp Exp
  | Or Exp Exp

  | Is Exp Pattern
  | In Exp Exp
  | Subset Exp Exp
  
  | Assign Exp Exp
  | PlusAssign Exp Exp
  | MinusAssign Exp Exp
  | MultAssign Exp Exp
  | DivAssign Exp Exp

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
  | Mod Exp Exp
  
  | Not Exp
  | Neg Exp
  
  | Dot Exp Exp
  
  | Parens Exp
  | BracksEmpty
  | Bracks Exp
  | BracesEmpty
  | Braces Exp
  | Bars Exp
  | ListItem Exp Exp
  | DictItem Exp Exp
  | IndexItem Exp [Exp]

  | Phrase [PhraseAtom]

  | FunApp Variable [Exp]
  | Tuple [Exp]
  | Lambda [Variable] Exp
  deriving Eq

data PhraseAtom =
    PhraseExp Exp
  | PhraseWord String
  deriving Eq

data Pattern =
    PatternVar String
  | PatternCon String [Pattern]
  | AnonPattern [Pattern]
  deriving Eq

----------------------------------------------------------------
-- Helper functions useful to the parser.

tuple :: [Exp] -> Exp
tuple [e] = e
tuple es = Tuple es

----------------------------------------------------------------
-- Printing functions for the abstract syntax.

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
  show e = let sh = show in case e of
    Int n -> show n
    Var x -> x
    ConApp s es -> if length es == 0 then s else s ++ "(" ++ (join ", " (map sh es)) ++ ")"
    Comma e1 e2 -> sh e1 ++ " in " ++ sh e2
    Ldots e1 e2 -> sh e1 ++ " ... " ++ sh e2
    Maps e1 e2 -> sh e1 ++ " :> " ++ sh e2
    And e1 e2 -> sh e1 ++ " and " ++ sh e2
    Or e1 e2 -> sh e1 ++ " or " ++ sh e2
    Is e p -> sh e ++ " is " ++ show p
    In e1 e2 -> sh e1 ++ " in " ++ sh e2
    Subset e1 e2 -> sh e1 ++ " subset " ++ sh e2
    Assign e1 e2 -> sh e1 ++ " := " ++ sh e2
    Eq e1 e2 -> sh e1 ++ " = " ++ sh e2
    Neq e1 e2 -> sh e1 ++ " != " ++ sh e2
    Lt e1 e2 -> sh e1 ++ " < " ++ sh e2
    Gt e1 e2 -> sh e1 ++ " > " ++ sh e2
    Leq e1 e2 -> sh e1 ++ " <= " ++ sh e2
    Geq e1 e2 -> sh e1 ++ " >= " ++ sh e2
    Union e1 e2 -> sh e1 ++ " union " ++ sh e2
    Intersect e1 e2 -> sh e1 ++ " intersect " ++ sh e2
    Max e -> "max " ++ sh e
    Min e -> "min " ++ sh e
    Domain e -> "domain " ++ sh e
    Codomain e -> "codomain " ++ sh e
    Plus e1 e2 -> "(" ++ sh e1 ++ " + " ++ sh e2 ++ ")"
    Minus e1 e2 -> "(" ++ sh e1 ++ " - " ++ sh e2 ++ ")"
    Mult e1 e2 -> "(" ++ sh e1 ++ " * " ++ sh e2 ++ ")"
    Div e1 e2 -> "(" ++ sh e1 ++ " / " ++ sh e2 ++ ")"
    Pow e1 e2 -> "(" ++ sh e1 ++ "^" ++ sh e2 ++ ")"
    Not e -> "not(" ++ sh e ++ ")"
    Neg e -> "-(" ++ sh e ++ ")"
    Dot e1 e2 -> sh e1 ++ "." ++ sh e2
    Parens e -> "(" ++ sh e ++ ")"
    Bracks e -> "[" ++ sh e ++ "]"
    Braces e -> "{" ++ sh e ++ "}"
    Bars e -> "|" ++ sh e ++ "|"
    BracksEmpty -> "[]"
    BracesEmpty -> "{}"
    Tuple es -> showTuple (map sh es)

instance Show Pattern where
  show p = case p of
    PatternVar x -> x
    PatternCon c ps -> if length ps == 0 then c else c ++ "(" ++ join ", " (map show ps) ++ ")"

showTuple :: [String] -> String
showTuple [] = "()"
showTuple [x] = x
showTuple xs = "(" ++ join ", " xs ++")"

showBlock indent b = case b of
  Block ls -> join "\n" $ map (showStmtLine indent) ls
  Stmt s   -> showStmt indent s

showStmtLine indent s = case s of
  StmtLine s -> showStmt indent s

showStmt indent s = case s of
  Function f xs b -> indent ++ "function" ++ " " ++ f ++ " (" ++ join ", " xs ++ ")" ++ "\n" ++ showBlock (indent++"  ") b
  For e b         -> indent ++ "for" ++ " " ++ show e ++ "\n" ++ showBlock (indent++"  ") b
  While e b       -> indent ++ "while" ++ " " ++ show e ++ "\n" ++ showBlock (indent++"  ") b
  If e b          -> indent ++ "if" ++ " " ++ show e ++ "\n" ++ showBlock (indent++"  ") b
  ElseIf e b      -> indent ++ "elseif" ++ " " ++ show e ++ "\n" ++ showBlock (indent++"  ") b
  Else b          -> indent ++ "else" ++ "\n" ++ showBlock (indent++"  ") b
  Global e        -> indent ++ "global" ++ " " ++ show e
  Local e         -> indent ++ "local" ++ " " ++ show e
  Return e        -> indent ++ "return" ++ " " ++ show e
  Continue        -> indent ++ "continue"
  Break           -> indent ++ "break"
  StmtExp e       -> indent ++ show e
  
--eof
