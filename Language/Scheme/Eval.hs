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
quote (n:[]) = return . SQuote $ n

lambda :: SBuiltin
lambda (binds:body:[]) = do
    env <- ask
    return $ SFunc (map getIdent . toList $ binds) env body

begin :: SBuiltin
begin = liftM last . mapM eval

set :: SBuiltin
set ((SIdent name):val:[]) = do
    env <- ask
    liftIO $ do
      mp <- env
      writeIORef (fromJust . Map.lookup name $ mp) val
    return SNil

let' :: SBuiltin
let' (binds:body:[]) = do
    env <- ask
    let (idents, vals) = unzip . map (\((SIdent ident):val:[]) -> (ident, val)) . map toList . toList $ binds
    (SFunc idents env body) `apply` vals

if' :: SBuiltin
if' (cond:e1:e2:[]) = do
    (SBool b) <- eval cond
    eval $ if b then e1 else e2

-- |Turns a Haskell predicate on a list of SData into a Scheme builtin
-- which evaluates the same predicate
predToBuiltin :: (SData -> Bool) -> SBuiltin
predToBuiltin pred = liftM (SBool . pred) . eval . head

predToBuiltin2 :: (SData -> SData -> Bool) -> SBuiltin
predToBuiltin2 pred (x:y:_) = liftM SBool $ liftM2 pred (eval x) (eval y)

builtins :: Map.Map String SBuiltin
builtins = Map.fromList [
             ("quote", quote),
             ("lambda", lambda),
             ("begin", begin),
             ("set!", set),
             ("let", let'),
             ("if", if'),

             ("procedure?", predToBuiltin isFunc),
             ("boolean?",   predToBuiltin isBool),
             ("pair?",      predToBuiltin isPair),
             ("list?",      predToBuiltin isList),
             ("symbol?",    predToBuiltin isIdent),
             ("string?",    predToBuiltin isString),

             (">",          predToBuiltin2 (>)),
             (">=",         predToBuiltin2 (>=)),
             ("<",          predToBuiltin2 (<)),
             ("<=",         predToBuiltin2 (<=))
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
