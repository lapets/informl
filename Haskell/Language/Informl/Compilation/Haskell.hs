----------------------------------------------------------------
--
-- Informl
-- Meta-language for quickly defining DSLs, including their
-- abstract syntax data structures, parsers, prettyprinters,
-- and other transformations.
--
-- Language/Informl/Compilation/Haskell.hs
--   Haskell implementation of the Informl language Haskell
--   compilation algorithm.
--

----------------------------------------------------------------
-- 

module Language.Informl.Compilation.Haskell
  where

import Data.String.Utils (join)
import Data.List.Utils (replace)

import Language.Informl.Compilation
import Language.Informl.AbstractSyntax

----------------------------------------------------------------
-- 

class ToHaskell a where
  compile :: a -> Compilation ()

instance ToHaskell Top where
  compile (Top m) = compile m

instance ToHaskell Module where
  compile (Module m ss) = 
    do raw $ "module " ++ m ++ " where"
       setModule m
       newline
       raw "import qualified UXADT as UXADT"
       newline
       raw "import qualified Informl as Informl"
       mapM compile ss
       nothing

instance ToHaskell StmtLine where
  compile (StmtLine s) = do {newline; compile s}

instance ToHaskell Block where
  compile b = case b of
    Stmt s -> do compile s
    Block ss ->
      do raw "do {"
         indent
         newline
         mapM compile ss
         unindent
         newline
         raw "}"

instance ToHaskell Stmt where
  compile s = case s of
    StmtExp e -> do { compile e; string ";" }
    _ -> do nothing

instance ToHaskell Exp where
  compile e = case e of
    _ -> do nothing

--eof
