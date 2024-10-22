module Main where

import Control.Monad (join, void)
import Control.Monad.Combinators.Expr
import Control.Monad.Identity (Identity, join)
import Data.Char (isAlphaNum, isDigit)
import Data.List (find, isPrefixOf, singleton, tails)
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Void
import System.Environment
import System.Exit
import Text.Megaparsec
import Text.Megaparsec.Char

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
      if regexMatch pattern input_line
        then exitSuccess
        else exitFailure

data Pattern
  = Empty
  | Literal Char
  | Digit
  | AlphaNum
  | Wildcard
  | PosCharClass String
  | NegCharClass String
  | StartAnchor
  | EndAnchor
  | Repeat Quantifier Pattern
  | Alternative Pattern Pattern
  | Capture Pattern
  | Reference Int
  | Seq [Pattern]
  deriving (Eq, Show)

-- Extension: Add Range Int Int constructor.
data Quantifier = ZeroOrOne | OneOrMore
  deriving (Eq, Show)

-- Parsec type parameters demystified: don't do anything fancy with erorrs, consume a String.
type Parser = Parsec Void String

specialChars :: String
specialChars = "$^+?[]|(){}."

-- Basic parsers.
-- These are the parsers for the basic elements of a regular expression, e.g. a literal
-- character like "a", the string ""\\d", character classes like "[asd]" or "[^asd]",
-- quantifiers "+" or "?", anchors "^" or "$" and the recursively defined alternative
-- "(cat|dog)" where "cat" could actually be any regular expression.

-- Literal expressions, like 'Hi there'.
-- Extension: Add labels like this to the other parsers to give richer feedback.
pLiteral :: Parser Pattern
pLiteral = Literal <$> satisfy (\c -> c `notElem` specialChars) <?> "non special character."

-- Patterns that begin with a slash, like '\w', '\d' or '\1' (a reference.)
pLeadingSlash :: Parser Pattern
pLeadingSlash = do
  void $ char '\\'
  maybeN <- optional (read <$> some (satisfy isDigit))
  case maybeN of
    Just n -> return $ Reference n
    Nothing -> do
      c <- anySingle
      case c of
        'w' -> return $ AlphaNum
        'd' -> return $ Digit

-- TODO: Escaped special chars.

-- Positive or negative character classes, like '[xyz]' or '[^xyz]'
pCharClass :: Parser Pattern
pCharClass = do
  void $ char '['
  negation <- optional $ char '^'
  chars <- some $ satisfy (\c -> c `notElem` specialChars)
  void $ char ']'
  case negation of
    Nothing -> return $ PosCharClass chars
    _ -> return $ NegCharClass chars

pStartAnchor :: Parser Pattern
pStartAnchor = StartAnchor <$ char '^'

pEndAnchor :: Parser Pattern
pEndAnchor = EndAnchor <$ char '$'

pQuantifier :: Parser Quantifier
pQuantifier = (ZeroOrOne <$ char '?') <|> (OneOrMore <$ char '+')

pWildcard :: Parser Pattern
pWildcard = Wildcard <$ char '.'

-- Captured groups, including alternatives like '(Hugo)' or '(left|right)'
pCapture :: Parser Pattern
pCapture = do
  void $ char '('
  (left, maybeRight) <- (,) <$> pRegex <*> (optional ((char '|') *> pRegex)) <* (char ')')
  return $ case maybeRight of
    Nothing -> Capture left
    Just right -> Capture (Alternative left right)

-- More involved parsers.
-- The full regex parser is built up in stages:
--   + parse anything that a single character can match against;
--   + allow a repetition quantifier (e.g. "+" or "?") to follow;
--   + parse any number of these into a sequence, and then allow the whole sequence
--     to be preceeded by the start anchor ("^").

-- An regular expression that does not have a repetition quantifier.
pUnquantified :: Parser Pattern
pUnquantified =
  choice
    [ pCharClass,
      pEndAnchor,
      pCapture,
      pWildcard,
      pLeadingSlash,
      pLiteral
    ]

-- A regex term, possibly with a postfix quantifier.
pTerm :: Parser Pattern
pTerm = applyQuantifier <$> pUnquantified <*> optional pQuantifier
  where
    applyQuantifier pattern Nothing = pattern
    applyQuantifier pattern (Just quantifier) = Repeat quantifier pattern

-- The final parser for a regular expression.
pRegex :: Parser Pattern
pRegex = unpackSeq <$> anchored
  where
    anchored = absorbPrefix <$> optional pStartAnchor <*> many pTerm
    unpackSeq [] = Empty
    unpackSeq [pattern] = pattern
    unpackSeq patterns = Seq patterns
    absorbPrefix Nothing xs = xs
    absorbPrefix (Just x) xs = x : xs

-- | Given a regular expression, some input and a list of captures, optionally
-- return a tuple holding the matched part, the remainder of the string and
-- a list of captures.
-- TODO: Are all base cases meaningful?
consume ::
  Pattern -> -- A parsed regular expression to match against.
  String -> -- The input to compare the expression to.
  [String] -> -- A list of the parts of the string matched by any previous captures (since the function is recursive)
  Maybe (String, String, [String]) -- A tuple containing the consumed part of the string, the rest and the parts of the string matched by any previous captures.
consume Empty input captures = Just ("", input, captures)
consume Wildcard "" _ = Nothing
consume Wildcard (s : sx) captures = Just ([s], sx, captures)
consume EndAnchor s captures = if null s then Just ("", "", captures) else Nothing
consume (Literal _) "" _ = Nothing
consume (Literal c) (s : sx) captures = if c == s then Just ([s], sx, captures) else Nothing
consume Digit "" _ = Nothing
consume Digit (s : sx) captures = if isDigit s then Just ([s], sx, captures) else Nothing
consume AlphaNum "" _ = Nothing
consume AlphaNum (s : sx) captures = if isAlphaNum s then Just ([s], sx, captures) else Nothing
consume (PosCharClass _) "" _ = Nothing
consume (PosCharClass chars) (s : sx) captures =
  if s `elem` chars then Just ([s], sx, captures) else Nothing
consume (NegCharClass chars) "" _ = Nothing
consume (NegCharClass chars) (s : sx) captures =
  if s `notElem` chars then Just ([s], sx, captures) else Nothing
-- TODO: This is not efficient as it checks shorter matches over and over.
consume (Repeat OneOrMore pattern) input captures = do
  -- If you have a pattern p, the repetitions of p are
  -- p, Seq [p, p], Seq [p, p, p] ...
  let patternReps = (map Seq $ (map (\n -> take n (repeat pattern)) [1 ..]))
  let bar = map (\p -> consume p input captures) patternReps
  (a, b, c) <- join . safeHead $ reverse (takeWhile isJust bar)
  return $ (a, b, captures)
consume (Repeat ZeroOrOne pattern) input captures =
  if isNothing match then Just ("", input, captures) else match
  where
    match = consume pattern input captures
consume (Seq []) input captures = Just ("", input, captures)
consume (Seq [pattern]) input captures = consume pattern input captures
consume (Seq (p : px)) input captures = do
  (consumed, remainder, captures') <- consume p input captures
  (a, b, c) <- consume (Seq px) remainder captures'
  return $ (consumed ++ a, b, c)
consume (Alternative left right) input captures = case consume left input captures of
  Just x -> Just x
  Nothing -> consume right input captures
consume (Capture pattern) input captures = do
  (consumed, remaining, captures') <- consume pattern input captures
  return $ (consumed, remaining, captures' ++ [consumed])
consume (Reference n) input captures =
  if capture `isPrefixOf` input
    then Just (capture, drop (length capture) input, captures)
    else Nothing
  where
    capture = (captures !! (n - 1))

regexMatch :: String -> String -> Bool
regexMatch pattern input = isJust $ do
  case parse pRegex "<stdin>" pattern of
    Left bundle -> Nothing
    -- If the pattern begins with a start anchor, then you must match from the stat of the string.
    Right (Seq (StartAnchor : patterns)) -> consume (Seq patterns) input []
    -- Otherwise, you can try to match the pattern starting anywhere.
    Right result -> join $ find isJust (map (\i -> consume result i []) (tails input))

-- TODO: BackRefs...
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead x = Just (head x)

-- WIP
-- A nested backref means a ref inside a capture.
-- Need to do a pass over the AST and convert any captures/backrefs to standard regex.

-- First put the captures in order, according to the order than they are encountered
-- when reading the pattern left to right.
g :: Pattern -> [Pattern]
-- If the first element is a capture, this capture is part of the return value.
g (Seq (Capture p : px)) = Capture p : g p ++ g (Seq px)
-- If the first element is not a capture, ignore it.
g (Seq (p : px)) = g (Seq px)
g (Seq _) = []
g _ = []
