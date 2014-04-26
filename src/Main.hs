{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.List
import Data.Array
import Data.List.Split
import Data.Char
import System.Directory
import System.Process
import System.Console.Readline
import Control.Monad
import Control.Monad.Trans
import Control.Lens

import Expr
import Eval
import Parser
import Interpreter
import Utils

data Command
    = PrintExpr Expr 
    | EvalProgram Program 
    | ChangeDir (Maybe String)
    | Scope
    | ClearScope
    | DelVars [String]
    | ShellCmd String
    | Load String
    | Debug String
    | Lines
    | Step
    | BreakLine Int
    | BreakExpr Expr
    | ShowBreakpoints
    | DeleteBreakpoints [Int]
    | Run 

showErrors :: String -> [Error] -> Eval ()
showErrors t es = liftIO $ putStrLn $ t ++ " error: " ++ (show es)

tryEvalOrShowErrors :: String -> Eval () -> Eval ()
tryEvalOrShowErrors t m = do
    r <- tryEval m
    either (showErrors t) return r

showCurrentLines :: Eval()
showCurrentLines = do
    fl <- use currentFile
    let (firstLine, lastLine) = bounds fl
    cl <- use currentLine
    if cl > lastLine then return () else do
        let ls = filter (\x -> x >= firstLine && x <= lastLine) $ map (+cl)  [-2..2]
        liftIO $ forM_ ls (printLine fl cl)
    where 
        printLine fl cl i = do
            let str = fl ! i
            let outStr = if i == cl then ("\ESC[33m" ++ str ++"\ESC[m")  else str
            putStrLn outStr

addBreakpoint :: Breakpoint -> Eval ()
addBreakpoint bp = do
   breakpoints %= (bp:) 

deleteNthBreakpoints :: [Int] -> Eval ()
deleteNthBreakpoints xs = do  
    bs <- use breakpoints
    let toSave = filter ( not . (flip elem xs) . fst) $ zip [0..] bs
    breakpoints .= map snd toSave 

command :: Command -> Eval ()

command (PrintExpr expr) = do
    x <- runInterpreter $ evalExpr expr

command (EvalProgram program) = do
    noDebug .= True
    liftIO $ print program
    runProgram $ evalProgram program 
    noDebug .= False

command (ChangeDir x) = do
    dir <- maybe (liftIO $ getHomeDirectory) return x
    liftIO $ setCurrentDirectory dir 

command Scope = scope >>= (liftIO . putStrLn . show)
command ClearScope = clearScope
command (DelVars vs) = forM_ vs deleteVar
command (ShellCmd cmd) = liftIO $ (system cmd >> return ())

command (Load file) = do
    src <- liftIO $ readFile file
    paused .= Nothing
    either (showErrors "Syntax") (command . EvalProgram) $ parseProgram src 

command (Debug file) = do
    src <- liftIO $ readFile file
    let fileLines = lines src
    
    currentFile .= listArray (0, (length fileLines) - 1) fileLines
    currentLine .= 0 
    
    either (showErrors "Syntax") eval' $ parseProgram src
    where
        eval' p = do
            paused .= (Just $ evalProgram p)

command (BreakLine l) = do
    addBreakpoint (Line l)

command (BreakExpr l) = do
    addBreakpoint (CExpr l)

command Lines = showCurrentLines
command ShowBreakpoints = do
    bs <- use breakpoints
    liftIO $ putStrLn (show $ zip (([1..]) :: [Int]) bs)

command (DeleteBreakpoints xs) = deleteNthBreakpoints xs

command Step = do
    p <- use paused
    evalOneStatement .= True
    maybe (return ()) runProgram p
    evalOneStatement .= False 

command Run = do
    (use paused) >>= (maybe (liftIO $ putStrLn "Nothing is debugged") runProgram)

request :: String -> Eval Command
request s = case s of
    (':':cmd) -> parseCmd cmd
    _ -> either (const parseProgram') (return . PrintExpr) $ parseExpr s
    where
        parseProgram' = either (fail . show) (return . EvalProgram) $ parseProgram s
        
        parseCmd (stripPrefix "cd" -> Just xs) = case xs of
            (' ':dir) -> return $ ChangeDir $ Just dir
            []        -> return $ ChangeDir Nothing
            r         -> fail ("Wrong command cd" ++ r)
        
        parseCmd (stripPrefix "del " -> Just vars) = do 
            if all (\c -> isDigit c || c == ' ') vars 
                then return $ DeleteBreakpoints $ map (\i -> (read i) - 1) $ splitOn " " vars 
                else return $ DelVars $ splitOn " " vars  
        
        parseCmd (stripPrefix "load " -> Just file) = return $ Load (trim file)
        parseCmd (stripPrefix "debug " -> Just file) = return $ Debug (trim file)
        
        parseCmd (stripPrefix "break " -> Just l) = do 
            if all isDigit l     
                then return $ BreakLine ((read l) - 1)
                else either (const $ fail "Syntax") (return . BreakExpr) $ parseExpr l
        
        parseCmd ('!':shellCmd) = return $ ShellCmd shellCmd

        parseCmd s' | s' == "scope" = return Scope 
                    | s' == "clear" = return ClearScope
                    | s' == "lines" = return Lines
                    | s' == "step"  = return Step
                    | s' == "show"  = return ShowBreakpoints
                    | s' == "run"  = return Run
                    | otherwise = fail ("Wrong command " ++ s')

runRepl :: Eval ()
runRepl = do
    mayBeS <- liftIO $ readline "> "
    case mayBeS of
        Just p -> (handleCommand p) >> runRepl
        Nothing -> (liftIO $ putStrLn "bye") >> return ()

handleCommand :: String -> Eval ()
handleCommand cmd = do
    liftIO $ addHistory cmd
    e <- tryEval $ request cmd
    case e of
        (Right x) -> tryEvalOrShowErrors "Runtime" $ command x
        (Left e') -> showErrors "Syntax" e' 

main :: IO ()
main = runEval runRepl

