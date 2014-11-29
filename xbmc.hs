module Main where

import System.Environment (getArgs)
import Network.Browser
import Network.HTTP

data Command = Unkown String
             | Status
             deriving (Show)

execute :: Command -> IO ()
execute (Unkown command)=
    putStrLn $ "Unkown command: " ++ command
execute Status = do
    putStrLn "Current status: great"
    (_, rsp) <- browse $ do
        setAllowRedirects True
        request $ getRequest "http://google.com/"
    print $ take 100 $ rspBody rsp

main :: IO ()
main = do
    commands <- getArgs
    if length commands == 0
        then putStrLn "usage blabla"
        else do
            let command = getCommand $ head commands
            putStrLn $ "Starting program with " ++ show command
            execute command

getCommand :: String -> Command
getCommand "status" = Status
getCommand command = Unkown command
