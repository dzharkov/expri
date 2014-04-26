module Interpreter
    ( runInterpreter, runProgram
     , evalExpr, evalStatement, evalProgram
    ) where

import Control.Monad
import Data.Maybe
import Expr
import Eval

import Control.Lens((.=), use)

pause :: Interpreter a -> Interpreter a
pause = Interpreter . return . Left 

liftEval :: Eval a -> Interpreter a
liftEval x = Interpreter $ x >>= (return . Right) 

getInt :: Interpreter Value -> Interpreter Integer
getInt m = do
    v <- m
    case v of
        (I x) -> return x
        _ -> fail "Int expected"
     
getBool :: Interpreter Value -> Interpreter Bool
getBool m = do 
    v <- m
    case v of
        (B x) -> return x
        _ -> fail "Bool expected"

if' :: Interpreter Value -> Interpreter () -> Maybe (Interpreter ()) -> Interpreter ()
if' c t e = do
    c' <- getBool c
    if c' then t else fromMaybe (return ()) e

evalExpr :: Expr -> Interpreter Value
evalExpr (Const v) = return v 
evalExpr (BinOp op a' b') = do
    let a = evalExpr a'
    let b = evalExpr b'
    case op of
       Plus -> liftM2 (\x y -> I $ x + y) (getInt a) (getInt b) 
       Mul -> liftM2 (\x y -> I $ x * y) (getInt a) (getInt b)
       Minus -> liftM2 (\x y -> I $ x - y) (getInt a) (getInt b)
       Less -> liftM2 (\x y -> B $ x < y) (getInt a) (getInt b)
       Greater -> liftM2 (\x y -> B $ x > y) (getInt a) (getInt b)
       Equals -> liftM2 (\x y -> B $ x == y) (getInt a) (getInt b)
       And -> liftM2 (\x y -> B $ x && y) (getBool a) (getBool b)
       Or -> liftM2 (\x y -> B $ x || y) (getBool a) (getBool b)

evalExpr (UnOp op v') = do
    let v = evalExpr v'
    case op of
        Not -> liftM (B . not) $ getBool v
        Neg -> liftM (I . negate) $ getInt v

evalExpr (Var s) = liftEval $ getVar s  

evalStatement :: LinedStatement -> Interpreter ()
evalStatement ls = evalStatement' $ st ls
    where
        evalStatement' (If c t e) = if' (evalExpr c) (evalProgram t) (fmap evalProgram e) 
        evalStatement' (While c p) = if' (evalExpr c) (evalProgram p >> evalLinedStatement ls) Nothing
        evalStatement' (Assign v e) = do
           eValue <- evalExpr e
           liftEval $ update v eValue

evalLinedStatement :: LinedStatement -> Interpreter ()
evalLinedStatement ls = do
    let curLine = line ls
    let nextStatement = evalStatement ls
    
    bs <- liftEval $ use breakpoints
    isDebug <- liftEval $ isDebugMode
    
    when (isDebug) $ liftEval $ currentLine .= curLine
    
    hasActiveBreakpoint <- (forM bs (isActiveBreakpoint curLine)) >>= (return . or) 
    isEvalOneStatement <- liftEval $ use evalOneStatement
    if isDebug && (hasActiveBreakpoint || isEvalOneStatement)  
        then pause nextStatement 
        else nextStatement
    where
        isActiveBreakpoint cl (Line l) = return $ cl == l
        isActiveBreakpoint _ (CExpr e) = evalBreakpointExpression e
        evalBreakpointExpression e = getBool $ (evalExpr e) `mplus` (return $ B False)

evalProgram :: Program -> Interpreter ()
evalProgram = flip forM_ $ evalLinedStatement  

runInterpreter :: Interpreter a -> Eval a
runInterpreter x = getEval x >>= (either (const $ fail "paused at expression") (return ))

runProgram :: Interpreter () -> Eval ()
runProgram x = getEval x >>= (either (\p -> (paused .= Just p) >> return ()) (return))

