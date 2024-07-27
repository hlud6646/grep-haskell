module Main where

import Data.Char (isDigit, isAlphaNum)
import System.Environment
import System.Exit

matchPattern :: String -> String -> Bool
matchPattern pattern input
  | length pattern == 1 = head pattern `elem` input
  | pattern == "\\d" = any isDigit input
  | pattern == "\\w" = any (\c -> isAlphaNum c || c == '_') input
  | otherwise = error $ "Unhandled pattern: " ++ pattern

main :: IO ()
main = do
  args <- getArgs
  let pattern = args !! 1
  input_line <- getLine

  if head args /= "-E"
    then do
      putStrLn "Expected first argument to be '-E'"
      exitFailure
    else do
      if matchPattern pattern input_line
        then exitSuccess
        else exitFailure
