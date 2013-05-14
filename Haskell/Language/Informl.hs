----------------------------------------------------------------
--
-- Informl
-- Meta-language for quickly defining DSLs, including their
-- abstract syntax data structures, parsers, prettyprinters,
-- and other transformations.
--
-- Language/Informl.hs
--   Haskell implementation of the Informl language parser and 
--   compilers.
--

----------------------------------------------------------------
-- Main module for the Haskell implementation of the
-- Informl language parser and compilers.

module Language.Informl
  where

import Data.List (splitAt, elemIndex)
import System.Environment (getArgs)
import System.IO
import Text.ParserCombinators.Parsec (ParseError)

import Language.Informl.AbstractSyntax (Top)
import Language.Informl.Parse (parseString)
import Language.Informl.Compilation
import qualified Language.Informl.Compilation.JavaScript as JS (compile)

----------------------------------------------------------------
-- The target of the output, as specified by the command-line
-- arguments.

data OutputTarget =
    JS
  | PHP
  | PY
  deriving Eq

----------------------------------------------------------------
-- Take a file path in the form of a string, and try to parse
-- the contents of the file into abstract syntax.

parseShow :: String -> IO ()
parseShow fname =
  do { s <- readFile fname
     ; r <- return $ parseString s
     ; case r of
         Left err -> do { putStr "parse error: "; print (err :: ParseError) }
         Right prgm ->
           do { putStr $ show prgm
              }
     }

parse :: String -> IO (Maybe Top)
parse str =
  do { r <- return $ parseString str
     ; case r of
         Left err -> do { putStr "parse error: "; print (err :: ParseError) ; return Nothing }
         Right top -> return $ Just top
     }

----------------------------------------------------------------
-- Take a file path in the form of a string, read it, and
-- process it as specified by the command line.

fileNamePrefix :: String -> String
fileNamePrefix s = fst $ splitAt (maybe (length s) id (elemIndex '.' s)) s

writeAndPutStr :: String -> String -> String -> IO ()
writeAndPutStr file ext s =
  do { writeFile (file++"."++ext) s
     ; putStr $ "  Wrote file \"" ++ file ++ "." ++ ext ++ "\".\n"
     }

procWrite :: [OutputTarget] -> Maybe String -> IO ()
procWrite outs fname =
  do { fname     <- maybe (return "") return fname
     ; txt       <- if length fname > 0 then readFile fname else return ""
     ; top       <- parse txt
     ; case top of
         Nothing -> return ()
         Just top ->
           do { fname <- return $ fileNamePrefix fname
              ; if JS `elem` outs then
                  do { js <- return $ extract $ JS.compile top
                     ; putStr $ "\n  Wrote file \"" ++ fname ++ ".js\".\n"
                     ; writeFile (fname++".js") $ js
                     }
                else
                  do return ()
              }
     }

usage :: IO ()
usage = putStr "\n  Usage:\tinforml [-js] [-php] [-py] \"path/file.ifl\"\n"

cmd :: [OutputTarget] -> [String] -> IO ()
cmd []      []            = usage
cmd outs    ("-js":args)  = cmd (JS:outs) args
cmd outs    ("-php":args) = cmd (PHP:outs) args
cmd outs    ("-py":args)  = cmd (PY:outs) args
cmd []      [f]           = usage
cmd outs    [f]           = procWrite outs (Just f)
cmd _ _                   = usage

--eof
