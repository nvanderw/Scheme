module Language.Scheme.Types (SData (..),
                              SEnv,
                              SBuiltin,
                              Scheme)

where

import qualified Data.Map as Map
import Control.Monad.Reader
import Data.IORef

-- |Monad representing the interpreter state
type Scheme env = ReaderT env IO

data SData = SPair SData SData
           | SInt {getInt :: Integer}
           | SBool {getBool :: Bool}
           | SString {getString :: String}
           | SIdent {getIdent :: String}
           | SFunc [String] SEnv SData
           | SQuote {getQuote :: SData}
           | SNil

-- The environment type screws with instance deriving, so we write our own:
instance Eq SData where
    SPair a b == SPair c d = (a == c) && (b == d)
    SInt a    == SInt b    = a == b
    SBool a   == SBool b   = a == b
    SString a == SString b = a == b
    SIdent a  == SIdent b  = a == b
    SQuote a  == SQuote b  = a == b
    SNil      == SNil      = True
    _         == _         = False

-- More boilerplate
instance Ord SData where
    SInt a    `compare` SInt b    = a `compare` b
    SBool a   `compare` SBool b   = a `compare` b
    SString a `compare` SString b = a `compare` b
    SIdent a  `compare` SIdent b  = a `compare` b
    SQuote a  `compare` SQuote b  = a `compare` b
    SNil      `compare` SNil      = EQ


-- |Bindings which are in scope
type SEnv = IO (Map.Map String (IORef SData))

-- |Type for builtin forms, which take a list of arguments and produce
-- Scheme data and perhaps some side-effects in the Scheme SEnv monad.
type SBuiltin = [SData] -> Scheme SEnv SData
