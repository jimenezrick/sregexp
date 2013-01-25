module Main (main) where

import Data.List
import System.IO
import System.Environment

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.Text as A

import Expression
import Matcher

main :: IO ()
main = do args <- getArgs
          prog <- getProgName
          case args of
            [pattern, file] -> do
                input <- B.readFile file
                case run pattern (TE.decodeUtf8 input) of
                  Left e   -> perror ["Error:", e]
                  Right rs -> presults rs
            _ ->
                perror ["Usage:", prog, "<pattern>", "<file>"]
    where run p i     = do re <- A.parseOnly regexp (T.pack p)
                           A.parseOnly (matcher re) i
          perror      = hPutStrLn stderr . (intercalate " ")
          presults rs = mapM_ (B.putStrLn . TE.encodeUtf8) rs
