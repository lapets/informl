----------------------------------------------------------------
--
-- Informl
-- Meta-language for quickly defining DSLs, including their
-- abstract syntax data structures, parsers, prettyprinters,
-- and other transformations.
--
-- Main.hs
--   Haskell implementatio of the Informl language parser and 
--   compilers.
--
--
-- * Usage:
--
--   informl [-js] [-php] [-py] "path/file.iml"
--
--
-- * Optional flags:
--
--   -js
--   Compile and emit JavaScript file.
--
--   -php
--   Compile and emit PHP file.
--
--   -py
--   Compile and emit Python 3 file.
--

----------------------------------------------------------------
-- Main module for the Haskell implementation of the
-- Informl language parser and compilers.

module Main
  where

import System.Environment (getArgs)
import System.IO

import Language.Informl

----------------------------------------------------------------
-- The main function.

main :: IO ()
main =
  do{ args <- getArgs
    ; cmd [] args
    }

--eof
