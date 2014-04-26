module ParserLib
    ( Parser
    , pure, (<$>), (<$), (<*>), (<*), (*>)
    , empty, (<|>)
    , satisfy, eof, lineNumber
    , evalParser
    ) where

import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Identity

newtype Parser a = Parser { runParser :: String -> StateT Int Identity (Maybe (a, String))}

evalParser :: Parser a -> String -> Maybe a
evalParser p s = fmap fst $ runIdentity $ evalStateT (runParser p s) 0 

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser parseChar
    where
        parseChar :: String -> StateT Int Identity (Maybe (Char, String))
        parseChar [] = return Nothing
        parseChar ('\n':xs) = do
            l <- get
            put $ l + 1
            parseChar (' ':xs)
        parseChar (x:xs) | p x = return $ Just (x, xs)
                         | otherwise = return $ Nothing

lineNumber :: Parser Int
lineNumber = Parser lineNumber'
    where
        lineNumber' s = do
            l <- get
            return $ Just (l,s) 

eof :: Parser ()
eof = Parser matchEmpty
    where
        matchEmpty [] = return $ Just ((), "")
        matchEmpty _ = return $ Nothing

instance Functor Parser where
    fmap g = (pure g <*>)

instance Applicative Parser where
    pure x = Parser $ (\s -> return $ Just (x, s))
    (<*>) p1 p2 = Parser apply
        where
            apply s = do
                r1 <- runParser p1 s
                case r1 of
                    Nothing -> return Nothing
                    Just (h,s') -> do 
                        r2 <- runParser p2 s'
                        case r2 of
                            Just (x,s'') -> return $ Just (h x, s'')
                            Nothing -> return Nothing

instance Alternative Parser where
    empty = Parser $ (\_ -> return Nothing)
    (<|>) p1 p2 = Parser apply
        where
            apply s = do
                l <- get
                r1 <- runParser p1 s
                case r1 of
                    Nothing -> (put l >> runParser p2 s)
                    x -> return x

