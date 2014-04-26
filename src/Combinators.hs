module Combinators
    ( module ParserLib
    , many, many1
    , char, anyChar, string, oneOf
    , digit, natural, integer
    , spaces
    , try
    , endBy, endBy1
    , sepBy, sepBy1
    , foldr1P, foldl1P
    , between, bSpaces, brackets, parens, braces, angles
    ) where

import ParserLib
import Data.Char

char :: Char -> Parser ()
char c = const () <$> satisfy (==c)  

anyChar :: Parser Char
anyChar = satisfy $ const True 

digit :: Parser Int
digit = (\c -> ord c - ord '0') <$> satisfy isDigit 

string :: String -> Parser ()
string [] = pure ()
string (x:xs) = char x *> string xs

oneOf :: String -> Parser Char
oneOf [] = empty 
oneOf (x:xs) = satisfy (==x) <|> oneOf xs 

many :: Parser a -> Parser [a]
many p = (:) <$> p <*> many p <|> pure []  

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p  

natural :: Parser Integer
natural = read <$> (many1 $ satisfy isDigit)  

integer :: Parser Integer
integer = negate <$> (char '-' *> natural) <|> natural 

spaces :: Parser ()
spaces = pure () <* (many $ char ' ')  

try :: Parser a -> Parser (Maybe a)
try p = Just <$> p <|> pure Nothing 

endBy :: Parser a -> Parser b -> Parser [a]
endBy p1 p2 = (:) <$> (p1 <* p2) <*> endBy p1 p2 <|> pure [] 

endBy1 :: Parser a -> Parser b -> Parser [a]
endBy1 p1 p2 = (:) <$> (p1 <* p2) <*> endBy p1 p2  

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p1 p2 = sepBy' <|> pure []
    where 
        sepBy' = (:) <$> p1 <*> (p2 *> sepBy' <|> pure []) 

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p1 p2 = sepBy1' 
    where
        sepBy1' = (:) <$> p1 <*> (p2 *> sepBy1' <|> pure[]) 


between :: Parser a -> Parser b -> Parser c -> Parser c
between p1 p2 p3 = p1 *> p3 <* p2 

bSpaces :: Parser a -> Parser a
bSpaces p = between spaces spaces p

pairs :: Char -> Char -> Parser a -> Parser a
pairs c1 c2 p = between (char c1) (char c2) p 

brackets :: Parser a -> Parser a
brackets = pairs '[' ']' 

parens :: Parser a -> Parser a
parens = pairs '(' ')' 

braces :: Parser a -> Parser a
braces = pairs '{' '}'

angles :: Parser a -> Parser a
angles = pairs '<' '>'

foldr1P :: (a -> b -> a -> a) -> Parser a -> Parser b -> Parser a
foldr1P f p1 p2 = f <$> p1 <*> p2 <*> (foldr1P f p1 p2) <|> p1   

foldl1P :: (a -> b -> a -> a) -> Parser a -> Parser b -> Parser a
foldl1P f p1 p2 = (\x g -> g x) <$> p1 <*> fold' <|> p1
    where
        fold' = (\b c g a -> g (f a b c)) <$> p2 <*> p1 <*> fold' <|> pure id

