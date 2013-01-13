{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment
import Control.Applicative
import Data.Attoparsec.Text (Parser, (<?>))

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A

data Regexp = Sequence [Regexp]
            | Literal String
            | Group Regexp
            deriving Show
             {-| Star-}
             {-| Plus-}

char :: Parser Char
char = esc <|> A.letter <?> "char"
    where esc = A.char '\\' *> (unescape <$> A.anyChar)

unescape :: Char -> Char
unescape 'n' = '\n'
unescape c   = c

regexp :: Parser Regexp
regexp = sqc <$> A.many1 (grp <|> lit) <?> "regexp"
    where grp   = Group <$> (A.char '(' *> regexp <* A.char ')')
          lit   = Literal <$> A.many1 char
          sqc l = case l of
                    [e] -> e -- Drop one element sequences
                    s   -> Sequence s

main :: IO ()
main = do as <- getArgs
          case as of
            [s] -> print $ A.parseOnly regexp $ T.pack s
            _   -> putStrLn "Give me one f*cking arg!"

-- e3:  literal | charclass | '.' | '^' | '$' | '(' e0 ')'
-- e2:  e3 | e2 REP
-- REP: '*' | '+' | '?'
-- e1:  e2 | e1 e2
-- e0:  e1 | e0 '|' e1
