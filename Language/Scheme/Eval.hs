module Language.Scheme.Eval where

import Prelude hiding (lookup)

import Data.List (foldr, unfoldr, intercalate)
import qualified Data.Map as Map
import Data.Maybe (isNothing, fromJust)
import Data.IORef
import Control.Monad
import Control.Monad.Reader

import Language.Scheme.Types
import Language.Scheme.Parser

-- |Convert from a list of Scheme objects to a Scheme list of those objects
fromList :: [SData] -> SData
fromList = foldr SPair SNil

-- |Inverse of fromList
toList :: SData -> [SData]
toList = unfoldr f
  where
    f SNil = Nothing
    f (SPair car cdr) = Just (car, cdr)

-- Some predicates
isPair :: SData -> Bool
isPair (SPair _ _) = True
isPair _ = False

isList :: SData -> Bool
isList SNil = True
isList (SPair car cdr) = isList cdr
isList _ = False

isInt :: SData -> Bool
isInt (SInt _) = True
isInt _ = False

isBool :: SData -> Bool
isBool (SBool _) = True
isBool _ = False

isString :: SData -> Bool
isString (SString _) = True
isString _ = False

isIdent :: SData -> Bool
isIdent (SIdent _) = True
isIdent _ = False

isFunc :: SData -> Bool
isFunc (SFunc _ _ _) = True
isFunc _ = False

isNil :: SData -> Bool
isNil SNil = True
isNil _ = False

-- Pretty-prints an SData
pretty :: SData -> String
pretty (SInt n) = show n
pretty (SBool b) = if b then "#t" else "#f"
pretty (SString s) = show s
pretty (SIdent s) = s
pretty (SFunc binds _ body) = "(lambda " ++ (pretty . fromList . map SIdent $ binds) ++
                              " " ++ pretty body ++ ")"
pretty (SQuote d) = pretty d
pretty xs = -- For SPair or SNil
    if isList xs
      then let ys = toList xs
             in "(" ++ (unwords . map pretty $ ys) ++ ")"
      else let (SPair a b) = xs
             in "(" ++ (pretty a) ++ " . " ++ (pretty b) ++ ")"

-- Builtins
quote :: SBuiltin
quote (n:_) = return . SQuote $ n

lambda :: SBuiltin
lambda (binds:body:_) = do
    env <- ask
    return $ SFunc (map getIdent . toList $ binds) env body

builtins :: Map.Map String SBuiltin
builtins = Map.fromList [
             ("quote", quote),
             ("lambda", lambda)
           ]

-- Looks up an identifier in the current environment
lookup :: String -> Scheme SEnv (Maybe SData)
lookup ident = do
    env <- ask
    liftIO $ do
      mp <- env
      let req = Map.lookup ident mp
      if isNothing req
        then return Nothing
        else liftM Just . readIORef . fromJust $ req

-- |Applies an SFunc to a list of arguments in the Scheme SEnv monad,
-- by evaluating the body in an environment which is the union of the
-- function's environment and the environment created by its bindings
apply :: SData -> [SData] -> Scheme SEnv SData
(SFunc binds env body) `apply` args = do
  refs <- liftIO $ mapM newIORef args
  e <- liftIO env -- Get environment of function as map
  local (\_ -> return (Map.fromList (zip binds refs) `Map.union` e)) (eval body) 

eval :: SData -> Scheme SEnv SData
eval d@(SInt n) = return d
eval d@(SBool b) = return d
eval d@(SString s) = return d
eval d@(SQuote q) = return q
eval d@(SIdent s) = liftM fromJust $ lookup s -- Errors if lookup fails
eval d = let (fun:args) = toList d
           in if (isIdent fun) && (Map.member (getIdent fun) builtins)
                -- Special forms
                then let (Just builtin) = Map.lookup (getIdent fun) builtins
                       in builtin args
                -- Normal function application
                else do
                  fun' <- eval fun
                  args' <- mapM eval args
                  apply fun' args'
