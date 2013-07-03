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
import qualified Language.Informl.Compilation.PHP as PHP (compile)
import qualified Language.Informl.Compilation.Python as PY (compile)
import qualified Language.Informl.Compilation.Haskell as HS (compile)

----------------------------------------------------------------
-- The target of the output, as specified by the command-line
-- arguments.

data OutputTarget =
    JS
  | PHP
  | PY
  | HS
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

writeAndPutStr :: OutputTarget -> [OutputTarget] -> String -> String -> String -> IO ()
writeAndPutStr ot ots file ext s =
  if ot `elem` ots then
    do { writeFile (file++"."++ext) s
       ; putStr $ "  Wrote file \"" ++ file ++ "." ++ ext ++ "\".\n"
       }
  else
    return ()

procWrite :: [OutputTarget] -> Maybe String -> IO ()
procWrite outs fname =
  do { fname     <- maybe (return "") return fname
     ; txt       <- if length fname > 0 then readFile fname else return ""
     ; top       <- parse txt
     ; case top of
         Nothing -> return ()
         Just top ->
           do { fname <- return $ fileNamePrefix fname
              ; putStr "\n"
              ; writeAndPutStr JS  outs fname "js"  (extract $ JS.compile top)
              ; writeAndPutStr PHP outs fname "php" (extract $ PHP.compile top)
              ; writeAndPutStr PY  outs fname "py"  (extract $ PY.compile top)
              ; writeAndPutStr HS  outs fname "hs"  (extract $ HS.compile top)
              }
     }

usage :: IO ()
usage = putStr "\n  Usage:\tinforml [-js] [-php] [-py] [-hs] \"path/file.iml\"\n"

cmd :: [OutputTarget] -> [String] -> IO ()
cmd []      []            = usage
cmd outs    ("-js":args)  = cmd (JS:outs) args
cmd outs    ("-php":args) = cmd (PHP:outs) args
cmd outs    ("-py":args)  = cmd (PY:outs) args
cmd outs    ("-hs":args)  = cmd (HS:outs) args
cmd []      [f]           = usage
cmd outs    [f]           = procWrite outs (Just f)
cmd _ _                   = usage

--eof
