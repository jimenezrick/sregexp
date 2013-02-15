module Matcher (matcher) where

import Control.Applicative
import Data.Attoparsec.Text (Parser)

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A

import Expression

matcher :: Regexp -> Parser [T.Text]
matcher re =
    case takeBol re of
      Nothing    -> loop re
      Just woBol -> do r <- matcher' woBol
                       if T.null r
                         then loop re
                         else (:) <$> pure r <*> loop re
    where loop re'    = skipTo re' *> (matches re' <|> retry re' <|> eof)
          matches re' = do r <- matcher' re'
                           if T.null r
                             then retry re'
                             else (:) <$> pure r <*> loop re'
          retry re'   = A.anyChar *> loop re'
          eof         = A.endOfInput *> pure []





-- XXX
takeBol :: Regexp -> Maybe Regexp
takeBol BOL              = Just $ Concat []
takeBol (Concat (BOL:res)) = Just $ Concat res
-- XXX





matcher' :: Regexp -> Parser T.Text
matcher' (Literal s)   = A.string $ T.pack s
matcher' (Range rs)    = T.singleton <$> (A.satisfy $ wire (||) $ map p rs)
    where p = \(c1, c2) -> \c -> c >= c1 && c <= c2
matcher' Dot           = T.singleton <$> A.anyChar
matcher' BOL           = A.char '\n' *> pure T.empty
matcher' EOL           = A.char '\n' *> pure T.empty <|> A.endOfInput *> pure T.empty
matcher' (Concat (re:res))
    | Star x <- re     = minMany (matcher' x) (matcher' $ Concat res)
    | Plus x <- re     = T.append <$> matcher' x <*> minMany (matcher' x) (matcher' $ Concat res)
    | otherwise        = T.append <$> matcher' re <*> (matcher' $ Concat res)
matcher' (Concat [])   = pure T.empty
matcher' (Group res)   = matcher' $ Concat [res]
matcher' (Optional re) = A.option T.empty $ matcher' re
matcher' (Star re)     = T.concat <$> (A.many' $ matcher' re)
matcher' (Plus re)     = T.concat <$> (A.many1 $ matcher' re)
matcher' (Or re1 re2)  = matcher' re1 <|> matcher' re2

minMany :: Parser T.Text -> Parser T.Text -> Parser T.Text
minMany r n = n <|> T.append <$> r <*> minMany r n

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
skipTo' (Concat [])    = []
skipTo' (Group res)    = skipTo' $ Concat [res]
skipTo' (Plus re)      = skipTo' re
skipTo' (Or re1 re2)   = skipTo' re1 ++ skipTo' re2
skipTo' _              = []

wire :: (b -> b -> b) -> [a -> b] -> (a -> b)
wire op preds = foldr1 f preds
    where f = \p p' -> \c -> p c `op` p' c
