module Main where

import Control.Applicative
import Data.Char
import Data.List (tails, singleton)
import System.Environment
import System.Exit

-- TODO: Remove deriving (Show) when complete.
-- TODO: Use megaparsec.

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

-- A parser which accepts any character.
anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

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

zeroOrOne :: Parser a -> Parser [a]
zeroOrOne p = (singleton <$> p) <|> pure []

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p



--
------------------------------------------------------------------------------------------------
--

data Element
  = Literal Char
  | Digit
  | AlphaNumeric
  deriving (Show)

data CharClass
  = PosCharClass [Char]
  | NegCharClass [Char]
  deriving (Show)

data Anchor
  = StartOfLine
  | EndOfLine
  deriving (Show)

data Quantifier
  = OneOrMore
  | ZeroOrOne
  deriving (Show)

data Matchable = E Element 
               | C CharClass
               | A Anchor
               | Q Quantifier
  deriving (Show)

-- 
------------------------------------------------------------------------------------------------
-- Parsers for regex. Look how compact applicative parsing can be.

nonSpecial :: Parser Char
nonSpecial = satisfy (\c -> c `notElem` "[]^$+?")

literal :: Parser Matchable
literal =  (E . Literal) <$> nonSpecial

digit :: Parser Matchable
digit = (E . \_ -> Digit) <$> (char '\\' *> char 'd')

alphanumeric :: Parser Matchable
alphanumeric = (E. \_ -> AlphaNumeric) <$> (char '\\' *> char 'w')

posCharClass :: Parser Matchable
posCharClass =  (C . PosCharClass) <$> (char '[' *> oneOrMore nonSpecial <* char ']')

negCharClass :: Parser Matchable
negCharClass = (C . NegCharClass) <$> (char '[' *> char '^' *> oneOrMore nonSpecial <* char ']')

startOfLine :: Parser Matchable
startOfLine = (A . \_ -> StartOfLine) <$> char '^'

endOfLine :: Parser Matchable
endOfLine = (A . \_ -> EndOfLine) <$> char '$'

-- Unfortunate naming conflift with the parsing utilities.
oneOrMore' :: Parser Matchable
oneOrMore' = (Q . \_ -> OneOrMore) <$> (char '+')

zeroOrOne' :: Parser Matchable
zeroOrOne' = (Q . \_ -> ZeroOrOne) <$> (char '?')

--
------------------------------------------------------------------------------------------------
-- Main bit.

-- A regex that does not contain anchors.
internal :: Parser [Matchable]
internal =  zeroOrMore
    (  alphanumeric
        <|> digit
        <|> posCharClass
        <|> negCharClass
        <|> oneOrMore'
        <|> zeroOrOne'
        <|> literal
    )



-- Can we kill this horrible prefix operator notation?
regex :: Parser [Matchable]
regex =
  (++) <$> ((++) <$> (zeroOrOne startOfLine) <*> internal) <*> (zeroOrOne endOfLine)

-- What happens if there are unparsed characters? 
-- This probably means that the parsing is not implemented properly, but maybe maybe we need to 
-- check here and return Nothing if there is something left?
parsePattern :: String -> Maybe [Matchable]
parsePattern pattern = fst <$> runParser regex pattern

-- Partial Function: Does not match anchors for example.
consume :: Matchable -> Char -> Bool
consume (E (Literal l)) c = l == c
consume (E Digit) c = isDigit c
consume (E AlphaNumeric) c = isAlphaNum c
consume (C (PosCharClass chars)) c = c `elem` chars
consume (C (NegCharClass chars)) c = c `notElem` chars

consumeAll :: [Matchable] -> String -> Bool
consumeAll [A EndOfLine] "" = True
consumeAll [A EndOfLine] _ = False
consumeAll [] _ = True
consumeAll m "" = False
consumeAll (m : (Q ZeroOrOne) : mx) (s : sx) =
  (consume m s && consumeAll mx sx) || (consumeAll mx (s : sx))
consumeAll (m : (Q OneOrMore) : mx) (s : sx) = 
  consume m s && consumeAll mx (dropWhile (consume m) sx)
consumeAll (e : ex) (s : sx) = consume e s && consumeAll ex sx










matchPattern :: String -> String -> Bool
matchPattern pattern input = case parsePattern pattern of
  Just (A StartOfLine : matchables) -> consumeAll matchables input 
  Just elements -> any (consumeAll elements) (tails input)
  Nothing -> error $ "Unhandled Pattern: " ++ pattern
















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
