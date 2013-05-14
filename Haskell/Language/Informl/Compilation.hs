----------------------------------------------------------------
--
-- Informl
-- Meta-language for quickly defining DSLs, including their
-- abstract syntax data structures, parsers, prettyprinters,
-- and other transformations.
--
-- Language/Informl/Compilation.hs
--   Haskell implementation of the Informl language compilation
--   monad.
--

----------------------------------------------------------------
-- Haskell implementation of the Informl language compilation
-- monad.

module Language.Informl.Compilation
  where

----------------------------------------------------------------
-- Data types and class memberships.

type FreshIndex = Integer
type Indentation = String
type ModuleName = String
type NestingDepth = Integer
type Raw = String
data State = State FreshIndex Indentation (Maybe ModuleName) NestingDepth Raw

empState = State 0 "" Nothing 0 ""

data Compilation a = Compilation (State -> (State, a))

-- Standard state monad definition.
instance Monad Compilation where
  return x = Compilation (\s -> (s, x))
  (>>=) (Compilation c1) fc2 = Compilation $
    (\state ->
      let (state', r) = c1 state
          Compilation c2 = fc2 r
      in c2 state'
    )

----------------------------------------------------------------
-- Combinators and functions.

extract :: Compilation () -> String
extract (Compilation c) = let (State _ _ _ _ s, _) = c empState in s

freshWithPrefix :: String -> Compilation String
freshWithPrefix p = Compilation $ \(State f i m n s) -> (State (f+1) i m n s, p ++ show f)

setModule :: String -> Compilation ()
setModule m = Compilation $ \(State f i _ n s) -> (State f i (Just m) n s, ())

getModule :: Compilation (Maybe String)
getModule = Compilation $ \(State f i m n s) -> (State f i m n s, m)

nothing :: Compilation ()
nothing = Compilation $ \s -> (s, ())

indent :: Compilation ()
indent = Compilation $ \(State f i m n s) -> (State f ("  " ++ i) m n s, ())

unindent :: Compilation ()
unindent = Compilation $ \(State f i m n s) -> (State f (drop (min (length i) 2) i) m n s, ())

newline :: Compilation ()
newline = Compilation $ \(State f i m n s) -> (State f i m n (s ++ "\n" ++ i), ())

string :: String -> Compilation ()
string s' = Compilation $ \(State f i m n s) -> (State f i m n (s ++ s'), ())

raw :: String -> Compilation ()
raw = string

nest :: Compilation ()
nest = Compilation $ \(State f i m n s) -> (State f i m (n+1) s, ())

unnest :: Compilation ()
unnest = Compilation $ \(State f i m n s) -> (State f i m (n-1) s, ())

depth :: Compilation Integer
depth = Compilation $ \(State f i m n s) -> (State f i m n s, n)

--eof
