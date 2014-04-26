module Parser
    ( parseExpr, parseStatement, parseProgram
    ) where

import Data.Char
import Expr
import ParserLib
import Combinators

boolP :: Parser Value 
boolP = (const $ B True) <$> string "True" <|> (const $ B False) <$> string "False" 

intP :: Parser Value
intP = I <$> integer

valueP :: Parser Value
valueP = boolP <|> intP

constP :: Parser Expr
constP = Const <$> valueP

identP :: Parser String
identP = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

---
anyP :: [(String, a)] -> Parser a
anyP = foldr1 (<|>) . map (\(s, a) -> (const a) <$> string s) 

foldBinOpP :: [(String, BinOp)] -> Parser Expr -> Parser Expr
foldBinOpP ops p = foldl1P (flip BinOp) (bSpaces p) (anyP ops) 
--

exprP' :: Parser Expr
exprP' = parens exprP <|> constP <|> Var <$> identP  

unOperators :: [([Char], UnOp)]
unOperators = [("-", Neg), ("!", Not)]

unOpP :: Parser Expr
unOpP = (UnOp <$> bSpaces (anyP unOperators) <*> exprP') <|> exprP'  

binOperators :: [[([Char], BinOp)]]
binOperators = [ [("*", Mul)], [("+", Plus), ("-", Minus)], [("<", Less), (">", Greater), ("==", Equals)], [("&&", And), ("||", Or)]]

binOpP :: Parser Expr
binOpP = binOpP' unOpP binOperators
    where 
        binOpP' e [] = e
        binOpP' e (x:xs) = binOpP' (foldBinOpP x e) xs

exprP :: Parser Expr
exprP = binOpP 

assignP :: Parser Statement
assignP = (const . id) <$> (Assign <$> bSpaces identP <* char '=' <*> exprP) <*> (bSpaces (char ';' <|> eof)) 

ifP :: Parser Statement
ifP = If <$> (bSpaces (string "if") *> parens exprP) <*> bSpaces (braces statementsP) <*> elseP

elseP :: Parser (Maybe Program) 
elseP = Just <$> (bSpaces (string "else") *> bSpaces (braces statementsP)) <|> pure Nothing 

whileP :: Parser Statement
whileP = While <$> (bSpaces (string "while") *> parens exprP) <*> bSpaces (braces statementsP)

statementP :: Parser Statement
statementP = assignP <|> ifP <|> whileP 

statementsP :: Parser Program
statementsP = many (bSpaces (LinedStatement <$> lineNumber <*> statementP))

parseLine :: Parser a -> String -> Either [String] a
parseLine p s = case evalParser (p <* eof) s of
    (Just x) -> Right x
    _        -> Left ["Error", s]

parseExpr :: String -> Either [String] Expr
parseExpr = parseLine exprP

parseStatement :: String -> Either [String] Statement
parseStatement = parseLine statementP

parseProgram :: String -> Either [String] Program
parseProgram = parseLine statementsP

