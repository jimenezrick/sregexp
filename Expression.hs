{-
   Regular expression's grammar:

   e0: e1 | e0 '|' e1
   e1: e2 | e1 e2
   e2: e4 | e2 e3
   e3: '?' | '*' | '+'
   e4: literal | charclass | '.' | '^' | '$' | '(' e0 ')'
-}

module Expression
    (
      Regexp(..)
    , regexp
    ) where

import Control.Applicative
import Data.Attoparsec.Text (Parser, (<?>))

import qualified Data.Attoparsec.Text as A

data Regexp = Literal String
            | Range [(Char, Char)] -- TODO: nextToken handle char's and Range cases
            | Dot
            | BOL
            | EOL
            | Concat [Regexp]
            | Group Regexp
            | Optional Regexp
            | Star Regexp
            | Plus Regexp
            | Or Regexp Regexp
            deriving Show

type RegexpOp = Regexp -> Regexp

char :: Parser Char
char = esc <|> chr <?> "char"
    where esc = A.char '\\' *> (unescape <$> A.anyChar)
          chr = A.satisfy $ not . isEspecialChar

isEspecialChar :: Char -> Bool
isEspecialChar c = c `elem` ['|', '?', '*', '+', '.', '^', '$', '(', ')', '[', ']']

unescape :: Char -> Char
unescape 't' = '\t'
unescape 'r' = '\r'
unescape 'n' = '\n'
unescape c   = c

regexp :: Parser Regexp
regexp = regexp0 <* A.endOfInput <?> "regexp"

regexp0 :: Parser Regexp
regexp0 = Or <$> regexp1 <*> (A.char '|' *> regexp0) <|> regexp1 <?> "regexp0"

regexp1 :: Parser Regexp
regexp1 = cat <$> A.many1 (regexp4 >>= regexp2) <?> "regexp1"

cat :: [Regexp] -> Regexp
cat [e] = e -- Prevent one element sequences
cat es  = Concat es

regexp2 :: Regexp -> Parser Regexp
regexp2 re = do x <- A.option Nothing (opt <|> star <|> plus) <?> "regexp2"
                case x of
                  Nothing -> pure re
                  Just op -> regexp2 $ insertOp op re
    where opt  = A.char '?' *> (pure $ Just Optional)
          star = A.char '*' *> (pure $ Just Star)
          plus = A.char '+' *> (pure $ Just Plus)

insertOp :: RegexpOp -> Regexp -> Regexp
insertOp op (Group re) = op re
insertOp op re         = op re

regexp4 :: Parser Regexp
regexp4 = lit <|> dot <|> bol <|> eol <|> grp <?> "regexp4"
    where lit = Literal <$> A.many1 char
          dot = A.char '.' *> pure Dot
          bol = A.char '^' *> pure BOL
          eol = A.char '$' *> pure EOL
          grp = Group <$> (A.char '(' *> regexp0 <* A.char ')')
