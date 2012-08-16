module Main (main) where

import Data.IORef
import qualified Data.Map as Map
import qualified Data.Text.Lazy.IO as Text

import Control.Monad.Reader

import Language.Scheme.Parser
import Language.Scheme.Eval

import System.IO

import Text.Parsec

main = do
    input <- Text.getContents
    case parse sexpr "stdin" input of
      Right parsed -> do
        result <- runReaderT (eval parsed) (return Map.empty)
        putStrLn . pretty $ result
      Left error -> hPutStrLn stderr . show $ error
