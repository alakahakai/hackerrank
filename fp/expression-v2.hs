{-
  Expression V2
  https://www.hackerrank.com/challenges/expressions-v2

  Author: Ray Qiu <ray.qiu@gmail.com>
  Date: June 14th, 2015
-}
import           Control.Applicative
import           Control.Monad
import           Data.Char           (isDigit, isSpace, ord)
import           Data.Maybe

newtype Parser a = Parser {
  parse :: String -> Maybe (a, String)
}

instance Functor Parser where
  fmap f (Parser p) = Parser (\cs -> case p cs of
                                 Nothing -> Nothing
                                 Just (a, cs') -> Just (f a, cs'))

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser (\cs -> Just (a, cs))
  p >>= f  = Parser (\cs -> do
                        (a,cs') <- parse p cs
                        parse (f a) cs')

instance Alternative Parser where
  empty = Parser (const Nothing)
  p <|> q = Parser (\cs -> case parse p cs of
                       Just r -> Just r
                       Nothing -> case parse q cs of
                                    Nothing -> empty
                                    Just r'' -> Just r'')

instance MonadPlus Parser where
  p `mplus` q = Parser (\cs -> parse p cs `mplus` parse q cs)
  mzero = Parser (const Nothing)

item :: Parser Char
item = Parser (\cs -> case cs of
                        [] -> Nothing
                        (a:cs) -> Just (a,cs))

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  a <- item
  if p a
    then return a
    else empty

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string [] = return []
string (c:cs) = do
  char c
  string cs
  return (c:cs)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> return []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = do
  a <- p
  as <- zeroOrMore p
  return (a:as)

int :: Parser Integer
int = Parser (\cs -> do
                 let (ns, rest) = span isDigit cs
                 case ns of
                   [] -> Nothing
                   r -> Just (read ns, rest))

space :: Parser String
space = zeroOrMore (satisfy isSpace)

token :: Parser a -> Parser a
token p = do
  space
  a <- p
  space
  return a

symb :: String -> Parser String
symb cs = token (string cs)

apply :: Parser a -> String -> Maybe (a,String)
apply p = parse (do space
                    p)

chainR :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainR p op a = (p `chainR1` op) <|> return a

chainR1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainR1 p op = do
  a <- p
  rest a where
    rest a = (do
      f <- op
      b <- p
      b' <- rest b
      return $ f a b') <|> return a

chainL :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainL p op a = (p `chainL1` op) <|> return a

chainL1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainL1 p op = do
    a <- p
    rest a where
      rest a = (do
        f <- op
        b <- p
        rest (f a b)) <|> return a

expr :: Parser Integer
expr = token term `chainR1` token addop

addop :: Parser (Integer -> Integer -> Integer)
addop = (do
  symb "+"
  return (+)) <|>
  (do
    symb "-"
    return (-))

powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b e m r
  | e == 0         = r
  | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
  | otherwise      = powm (b * b `mod` m) (e `div` 2) m r

mulop :: Parser (Integer -> Integer -> Integer)
mulop =
  (do
    symb "*"
    return (*))
  <|>
  (do
    symb "/"
    return (\x y -> x * powm y (10^9 + 5) (10^9 + 7) 1))

term :: Parser Integer
term = token factor `chainR1` token mulop

factor :: Parser Integer
factor = number <|>
  (do
    op <- symb "-"
    symb "("
    n <- expr
    symb ")"
    return (-n)) <|>
  (do
    symb "("
    n <- expr
    symb ")"
    return n)

number :: Parser Integer
number = int <|>
  (do
    op <- symb "-"
    n <- token int
    return (-n))

main :: IO ()
main = do
  ex <- getLine
  print . flip mod (10^9 + 7) . fst . fromJust $ apply expr ex
