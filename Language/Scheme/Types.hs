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

-- |Bindings which are in scope
type SEnv = IO (Map.Map String (IORef SData))

-- |Type for builtin forms, which take a list of arguments and produce
-- Scheme data and perhaps some side-effects in the Scheme SEnv monad.
type SBuiltin = [SData] -> Scheme SEnv SData
