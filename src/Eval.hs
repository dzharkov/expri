{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Eval
    ( Eval, runEval, tryEval
    , Interpreter(..), Breakpoint(..)
    , Error, Store
    , update, getVar, scope, clearScope, deleteVar
    , currentFile, currentLine, paused, evalOneStatement
    , breakpoints, isDebugMode, noDebug
    ) where

import qualified Data.Map as M
import Data.Array
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Trans.State as TS
import Control.Monad.State.Class(MonadState, put, get) 
import Control.Lens
import System.IO.Error

import Expr

newtype Eval a = Eval { evalRun :: TS.StateT Store IO (Maybe a, [Error]) }

newtype Interpreter a = Interpreter { getEval :: Eval (Either (Interpreter a) a) }

data Breakpoint = Line Int | CExpr Expr deriving Show

type Error = String
data Store = Store { _vars :: M.Map String Value, 
                     _paused :: Maybe (Interpreter ()), 
                     _currentLine :: Int, 
                     _currentFile :: Array Int String,
                     _evalOneStatement :: Bool,
                     _breakpoints :: [Breakpoint],
                     _noDebug :: Bool
                   }

$(makeLenses ''Store)

instance Monad Eval where
    return x = Eval (return (Just x, []))
    fail e = Eval (return (Nothing, [e]))
    Eval m >>= k = Eval bind'
        where
            bind' = do
                r <- m
                case r of
                    (Just x, e) -> evalRun (k x) >>= (\(y, e') -> return (y, e ++ e'))
                    (_, e) -> return (Nothing, e) 

instance MonadPlus Eval where
    mzero = Eval $ return (Nothing, []) 
    mplus (Eval l) (Eval r) = Eval mplus'
        where
            mplus' = do
                x <- l
                case x of
                    (Nothing, e) -> r >>= (\(x', e'') -> return (x', e ++ e'')) 
                    x' -> return x'  

instance MonadIO Eval where
    liftIO x = Eval $ liftIO lift'
        where
            lift' = do 
                r <- tryIOError x
                case r of
                    Left e -> return (Nothing, [ioeGetErrorString e])
                    Right x' -> return (Just x', [])

instance MonadState Store Eval where
    put s = Eval $ do
        TS.put s
        return (Just (), [])

    get = Eval $ do
        x <- TS.get
        return (Just x, [])

instance Monad Interpreter where
    return = Interpreter . return . Right
    fail = Interpreter . fail
    m >>= k = Interpreter ((getEval m) >>= process)
        where
            process (Left x) = return $ Left $ x >>= k 
            process (Right x) = getEval $ k x

instance MonadPlus Interpreter where
    mzero = Interpreter mzero 
    mplus a b = Interpreter $ mplus (getEval a) (getEval b)

isDebugMode :: Eval Bool
isDebugMode = do
    isNoDebug <- use noDebug
    uses paused $ maybe False $ const $ True && (not isNoDebug)     

scope :: Eval [(String, Value)]
scope = Eval scope'
    where
        scope' = do
            s <- use vars
            return (Just $ M.toList s, [])

clearScope :: Eval ()
clearScope = Eval clearScope'
    where
        clearScope' = do
            vars .= M.empty
            return (Just (), [])

deleteVar :: String -> Eval ()
deleteVar v = Eval deleteVar'
    where
        deleteVar' = do
            vars %= M.delete v
            return (Just (), []) 

update :: String -> Value -> Eval ()
update k v = Eval update'
    where
        update' = do
            vars %= M.insert k v 
            return (Just (), []) 

getVar :: String -> Eval Value
getVar v = Eval getVar'
    where
        getVar' = do
            s <- use vars
            return $ maybe (Nothing, [v ++ " not found"]) (\x -> (Just x, [])) $ s ^.at v

tryEval :: Eval a -> Eval (Either [Error] a)
tryEval m = Eval tryEval'
    where
        tryEval' = do
            r <- evalRun m
            return $ case r of 
                (Nothing, e) -> (Just $ Left e, [])
                (Just x, e) -> (Just $ Right x, e)

runEval :: Eval a -> IO ()
runEval m = do
    (_, __) <- TS.runStateT (evalRun m) (Store M.empty Nothing 0 (listArray (0,-1) []) False [] False)
    return ()

