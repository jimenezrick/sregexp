{-
   Regular expression's grammar:

   e0: e1 | e0 '|' e1
   e1: e2 | e1 e2
   e2: e4 | e2 e3
   e3: '?' | '*' | '+'
   e4: literal | charclass | '.' | '^' | '$' | '(' e0 ')'
-}

module Main (main) where

import System.Environment
import Control.Applicative
import Data.Attoparsec.Text (Parser, (<?>))

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A

data Regexp = Concat [Regexp]
            | Group Regexp
            | Literal String
            | Range Char Char
            | Dot
            | BOL
            | EOL
            | Optional Regexp
            | Star Regexp
            | Plus Regexp
            | Or Regexp Regexp
            deriving Show

type RegexpOp = Regexp -> Regexp

char :: Parser Char
char = chr <|> esc <?> "char"
    where chr = A.satisfy $ not . isEspecialChar
          esc = A.char '\\' *> (unescape <$> A.anyChar)

isEspecialChar :: Char -> Bool
isEspecialChar c = c `elem` ['|', '?', '*', '+', '.', '^', '$', '(', ')', '[', ']']

unescape :: Char -> Char
unescape 't' = '\t'
unescape 'r' = '\r'
unescape 'n' = '\n'
unescape c   = c

regexp :: Parser Regexp
regexp = cat <$> A.many1 (regexp4 >>= regexp2) <?> "regexp"

cat :: [Regexp] -> Regexp
cat [e] = e -- Prevent one element sequences
cat es  = Concat es

regexp2 :: Regexp -> Parser Regexp
regexp2 re = do x <- A.option Nothing (opt <|> star <|> plus)
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
          grp = Group <$> (A.char '(' *> regexp <* A.char ')')

main :: IO ()
main = do as <- getArgs
          case as of
            [s] -> print $ A.parseOnly regexp $ T.pack s
            _   -> putStrLn "Give me one f*cking arg!"
