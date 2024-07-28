module Main where

import Control.Applicative
import Data.Char
import Data.List (tails)
import System.Environment
import System.Exit

--
------------------------------------------------------------------------------------------------
-- Parsing. This is a toy version until I can fit megaparsec in.

-- A parser is a function which takes a String of input to be parsed,
-- which then succeeds or fails.
-- If it succeeds, it returns the parsed value and the remainder of the string.
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

-- Take a predicate on a character and return a parser which succeeds if the predicate is true.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x : xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

-- A parser which expects to see a given character.
char :: Char -> Parser Char
char c = satisfy (== c)

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
  fmap f (Parser g) = Parser (fmap (first f) . g)

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  (<*>) (Parser f) (Parser g) =
    Parser
      ( \s -> case f s of
          Just (h, rest) -> fmap (first h) (g rest)
          _ -> Nothing
      )

instance Alternative Parser where
  empty = Parser (const Nothing)
  (<|>) (Parser f) (Parser g) = Parser (liftA2 (<|>) f g)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

--
------------------------------------------------------------------------------------------------
-- Data types for regex elements. I have no idea what I'm doing with this, so hopefully it
-- generalises well enough.
--

-- The special characters in our regex program are \d, \w, ^, $, [], +, *, . and |
data Element = Literal Char
             | Digit
  deriving (Show)

literal :: Parser Element
literal = Parser f
  where
    f pattern
      | null pattern = Nothing
      | otherwise = Just (Literal $ head pattern, tail pattern)

digit :: Parser Element
digit = Parser f
  where
    f pattern
      | length pattern < 2 || take 2 pattern /= "\\d" = Nothing
      | otherwise = Just (Digit, drop 2 pattern)

--
------------------------------------------------------------------------------------------------
-- Main bit.

regex :: Parser [Element]
regex = oneOrMore (digit <|> literal)

parsePattern :: String -> Maybe [Element]
parsePattern pattern = fst <$> runParser regex pattern

consume :: Element -> Char -> Bool
consume (Literal l) c = l == c
consume Digit c = isDigit c

consumeAll :: [Element] -> String -> Bool
-- The empty pattern always matches.
consumeAll [] _ = True
-- A line here for the base case grep -e "^" ""
-- that is matching the start line or end line against the empty string...
-- Matching a pattern (assuming it's not the line start or end) against a non empty string will fail.
consumeAll e "" = False
consumeAll (e : ex) (s : sx) = consume e s && consumeAll ex sx


matchPattern :: String -> String -> Bool
matchPattern pattern input = case parsePattern pattern of
  -- Just elements -> consumeAll elements input
  Just elements -> any (consumeAll elements) (tails input)
  Nothing -> error $ "Unhandled Pattern: " ++ pattern


-- matchPattern :: String -> String -> Bool
-- matchPattern pattern input
--   | length pattern == 1 = head pattern `elem` input
--   | pattern == "\\d" = any isDigit input
--   | pattern == "\\w" = any (\c -> isAlphaNum c || c == '_') input
--   | otherwise = error $ "Unhandled pattern: " ++ pattern

-- matchPattern :: String -> String -> Bool
-- matchPattern pattern input = True where
--   regex = runParser parseRegex pattern

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
