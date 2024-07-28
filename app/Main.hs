module Main where

import Data.Char (isDigit, isAlphaNum)
import System.Environment
import System.Exit


isPositiveCharGroup :: String -> Bool
isPositiveCharGroup pattern = head pattern == '[' && last pattern == ']'

matchPositiveCharGroup :: String -> String -> Bool 
matchPositiveCharGroup pattern input =
  -- Build a list of single charaters or character group shorthands like \d or \w.
  any (\c -> matchPattern c input) (buildList . dropBrackets $ pattern)
  where buildList chars | length chars == 0 = []
                        | length chars == 1 = [chars]
                        | head chars == '\\' = (take 2 chars) : (buildList $ drop 2 chars)
                        | otherwise = [head chars] : (buildList $ tail chars)
        dropBrackets pattern = take (length pattern - 2) (tail pattern)
matchPattern :: String -> String -> Bool
matchPattern pattern input
  | length pattern == 1 = head pattern `elem` input
  | pattern == "\\d" = any isDigit input
  | pattern == "\\w" = any (\c -> isAlphaNum c || c == '_') input
  | isPositiveCharGroup pattern = matchPositiveCharGroup pattern input
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
