module Matcher (matcher) where

-- XXX:   Use Attoparsec for ByteString!
-- FIXME: Si tenemos un ^ lo primero de todo, poner un \n al princpio
--        para que lo consuma, sin el skipTo

import System.IO
import Data.Monoid
import Control.Applicative
import Data.Attoparsec.Text (Parser)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Attoparsec.Text as A

import Expression

matcher :: Regexp -> Parser [T.Text]
matcher re = skipTo re *> (matches <|> retry <|> eof)
    where matches = (:) <$> matcher' re <*> matcher re
          retry   = A.anyChar *> matcher re
          eof     = A.endOfInput *> pure []

matcher' :: Regexp -> Parser T.Text
matcher' (Literal s)   = A.string $ T.pack s
matcher' (Range rs)    = T.singleton <$> (A.satisfy $ wire (||) $ map p rs)
    where p = \(c1, c2) -> \c -> c >= c1 && c <= c2
matcher' Dot           = T.singleton <$> A.anyChar
matcher' BOL           = A.char '\n' *> pure T.empty -- FIXME: Doesn't match input beginning
matcher' EOL           = A.char '\n' *> pure T.empty <|> A.endOfInput *> pure T.empty
matcher' (Concat res)   = T.concat <$> (sequence $ map matcher' res)
matcher' (Group _)     = error "matcher': invalid argument"
matcher' (Optional re) = A.option T.empty $ matcher' re
matcher' (Star re)     = T.concat <$> (A.many' $ matcher' re)
matcher' (Plus re)     = T.concat <$> (A.many1 $ matcher' re)
matcher' (Or re1 re2)  = matcher' re1 <|> matcher' re2

skipTo :: Regexp -> Parser ()
skipTo re = case skipTo' re of
              []    -> pure ()
              preds -> A.skipWhile $ wire (&&) preds

skipTo' :: Regexp -> [Char -> Bool]
skipTo' (Literal s)    = [(head s /=)]
skipTo' (Range rs)     = map p rs
    where p = \(c1, c2) -> \c -> c < c1 || c > c2
skipTo' BOL            = [('\n' /=)]
skipTo' EOL            = [('\n' /=)]
skipTo' (Concat (re:res))
    | Optional x <- re = skipTo' x ++ (skipTo' $ Concat res)
    | Star x <- re     = skipTo' x ++ (skipTo' $ Concat res)
    | otherwise        = skipTo' re
skipTo' (Concat [])    = mempty
skipTo' (Group _)      = error "skipTo': invalid argument"
skipTo' (Plus re)      = skipTo' re
skipTo' (Or re1 re2)   = skipTo' re1 ++ skipTo' re2
skipTo' _              = mempty

wire :: (b -> b -> b) -> [a -> b] -> (a -> b)
wire op preds = foldr1 f preds
    where f = \p p' -> \c -> p c `op` p' c
