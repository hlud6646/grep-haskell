module Main where

import Data.Char (isDigit, isAlphaNum)
import System.Environment
import System.Exit


type Pattern = String


isPositiveCharGroup :: Pattern -> Bool
isPositiveCharGroup pattern = head pattern == '[' && last pattern == ']'

isNegativeCharGroup :: Pattern -> Bool
isNegativeCharGroup p | length p < 4 || take 2 p /= "[^" = False
                      | otherwise = last p == ']'

matchPositiveCharGroup :: Pattern -> String -> Bool
matchPositiveCharGroup pattern input =
  -- Build a list of single charaters or character group shorthands like \d or \w.
  any (`matchPattern` input) (buildList . dropBrackets $ pattern)
  where buildList chars | null chars = []
                        | length chars == 1 = [chars]
                        | head chars == '\\' = take 2 chars : buildList (drop 2 chars)
                        | otherwise = [head chars] : buildList (tail chars)
        dropBrackets pattern = take (length pattern - 2) (tail pattern)

matchPattern :: Pattern -> String -> Bool
matchPattern pattern input
  | length pattern == 1 = head pattern `elem` input
  | pattern == "\\d" = any isDigit input
  | pattern == "\\w" = any (\c -> isAlphaNum c || c == '_') input
  | isNegativeCharGroup pattern = not $ matchPositiveCharGroup pattern input
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



