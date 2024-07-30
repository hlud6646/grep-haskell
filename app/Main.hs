module Main where


import Control.Monad (void, join)
import Control.Monad.Combinators.Expr
import Control.Monad.Identity (Identity, join)
import Data.Char (isAlphaNum, isDigit)
import Data.List (tails, singleton, find)
import Data.Void
import Data.Maybe (isJust, listToMaybe)
import System.Environment
import System.Exit
import Text.Megaparsec
import Text.Megaparsec.Char

-- gadt refactor?
data Pattern
  = Empty
  | Literal Char
  | Digit
  | AlphaNum
  | PosCharClass String
  | NegCharClass String
  | StartAnchor
  | EndAnchor
  | Repeat Quantifier Pattern
  | Wildcard
  | Alternative Pattern Pattern
  | Seq [Pattern] -- COMBination
  deriving (Eq, Show)

-- Extension: Add Range Int Int constructor.
data Quantifier = ZeroOrOne | OneOrMore
  deriving (Eq, Show)

-- Parsec type parameters demystified: don't do anything fancy with erorrs, consume a String.
type Parser = Parsec Void String

regularChar :: Char -> Bool
regularChar c = c `notElem` "$^+?[]|()."


-- Basic parsers. 
-- These are the parsers for the basic elements of a regular expression, e.g. a literal
-- character like "a", the string ""\\d", character classes like "[asd]" or "[^asd]", 
-- quantifiers "+" or "?", anchors "^" or "$" and the recursively defined alternative 
-- "(cat|dog)" where "cat" could actually be any regular expression.

-- Extension: Add labels like this to the other parsers to give richer feedback.
pLiteral :: Parser Pattern
pLiteral = Literal <$> satisfy regularChar <?> "non special character."

pDigit :: Parser Pattern
pDigit = Digit <$ chunk "\\d"

pAlphaNum :: Parser Pattern
pAlphaNum = AlphaNum <$ chunk "\\w"

pPosCharClass :: Parser Pattern
pPosCharClass = do
  void (char '[')
  chars <- some (satisfy regularChar)
  void (char ']')
  return $ PosCharClass chars

pNegCharClass :: Parser Pattern
pNegCharClass = do
  void (chunk "[^")
  chars <- some (satisfy regularChar)
  void (char ']')
  return $ NegCharClass chars

pStartAnchor :: Parser Pattern
pStartAnchor = StartAnchor <$ char '^'

pEndAnchor :: Parser Pattern
pEndAnchor = EndAnchor <$ char '$'

pZeroOrOne :: Parser Quantifier
pZeroOrOne = ZeroOrOne <$ char '?'

pOneOrMore :: Parser Quantifier
pOneOrMore = OneOrMore <$ char '+'

pQuantifier :: Parser Quantifier
pQuantifier = pZeroOrOne <|> pOneOrMore

pAlternative :: Parser Pattern
pAlternative = do
  void (char '(')
  left <- pRegex
  void (char '|')
  right <- pRegex
  void (char ')')
  return $ Alternative left right

pWildCard :: Parser Pattern
pWildCard = Wildcard <$ char '.'
-- More involved parsers.
-- The full regex parser is built up in stages:
--   + parse anything that a single character can match against;
--   + allow a repetition quantifier (e.g. "+" or "?") to follow;
--   + parse any number of these into a sequence, and then allow the whole sequence 
--     to be preceeded by the start anchor ("^").

pTerm :: Parser Pattern
pTerm =
  choice
    [ pDigit,
      pAlphaNum,
      pNegCharClass,
      pPosCharClass,
      pAlternative,
      pLiteral,
      pEndAnchor,
      pWildCard
    -- TODO: Do we need pWildcard here?
    ]

-- Parse a Term as above, but pick up a repetition if it's given.
pQuantifiedTerm :: Parser Pattern
pQuantifiedTerm = applyQuantifier <$> pTerm <*> optional pQuantifier
  where
    applyQuantifier p Nothing = p
    applyQuantifier p (Just q) = Repeat q p

-- TODO:This needs a better name.
-- helper function to unpack empty or singleton lists to the appropriate constructor.
singletonOrSeq :: [Pattern] -> Pattern
singletonOrSeq [] = Empty
singletonOrSeq [pattern] = pattern
singletonOrSeq patterns = Seq patterns

pRegex :: Parser Pattern
pRegex = singletonOrSeq <$> anchored
  where
    anchored = applyStartAnchor <$> optional pStartAnchor <*> many pQuantifiedTerm
    -- If 'many' finds zero or one term, no need to wrap in a Comb.
    applyStartAnchor Nothing patterns = patterns
    applyStartAnchor (Just StartAnchor) patterns = StartAnchor : patterns

-- Matching an Input

-- All the base cases for now, trim the fat later.
--   pattern   input     remaining input
f :: Pattern -> String -> Maybe String
f Empty s = Just s
f Wildcard "" = Nothing
f Wildcard (s:sx) = Just sx
f EndAnchor s = if null s then Just "" else Nothing
f (Literal _) "" = Nothing
f (Literal c) (s:sx) = if c == s then Just sx else Nothing
f Digit "" = Nothing
f Digit (s:sx) = if isDigit s then Just sx else Nothing
f AlphaNum "" = Nothing
f AlphaNum (s:sx) = if isAlphaNum s then Just sx else Nothing
f (PosCharClass cx) "" = Nothing
f (PosCharClass cx) (s:sx) = if s `elem` cx then Just sx else Nothing
f (NegCharClass cx) "" = Nothing
f (NegCharClass cx) (s:sx) = if s `notElem` cx then Just sx else Nothing
f (Repeat OneOrMore p) s = do
  sx <- f p s
  return $ dropWhile (isJust . f p . singleton) sx
f (Repeat ZeroOrOne p) s = if isNothing match then Just s else match
  where match = f p s
f (Seq []) s = Just s
f (Seq [p]) s = f p s
f (Seq (p:px)) s = do
  sx <- f p s
  f (Seq px) sx
f (Alternative left right) s = case f left s of
  Just sx -> Just sx
  Nothing -> f right s

g :: String -> String -> Maybe String
g pattern input = do
  case parse pRegex "<stdin>" pattern of
    Left bundle -> Nothing
    Right (Seq (StartAnchor : patterns)) -> f (Seq patterns) input
    Right result -> join $ find isJust (map (f result) (tails input))

matchPattern :: String -> String -> Bool
matchPattern pattern input = isJust $ g pattern input

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
