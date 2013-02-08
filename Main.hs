{-# LANGUAGE OverloadedStrings #-}

module Main
    (
      main
    , mainDbg
    ) where

import Data.List
import System.IO
import System.Environment
import Control.Applicative

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.Text as A

import Expression
import Matcher

main :: IO (Maybe Regexp)
main = do args <- getArgs
          prog <- getProgName
          case args of
            [pattern, file] -> do
                input <- B.readFile file
                case run pattern (TE.decodeUtf8 input) of
                  Left e         -> do perror ["Error:", e]
                                       return Nothing
                  Right (re, rs) -> do presults rs
                                       return $ Just re
            _ -> do perror ["Usage:", prog, "<pattern>", "<file>"]
                    return Nothing
    where run p i     = do re <- A.parseOnly regexp (T.pack p)
                           (,) <$> Right re <*> A.parseOnly (matcher re) i
          perror      = hPutStrLn stderr . (intercalate " ")
          presults rs = B.putStr $ B.intercalate "\n--\n" $ map TE.encodeUtf8 rs

mainDbg :: [String] -> IO ()
mainDbg args = do Just re <- withArgs args main
                  print re
