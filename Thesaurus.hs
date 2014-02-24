{-# LANGUAGE OverloadedStrings #-}

module Thesaurus where

import           Control.Applicative     (many, (<$>), (<*>), (<|>))
import           Control.Arrow           ((&&&))
import           Prelude                 hiding (readFile)

import           Data.Attoparsec.Text    (Parser, char, digit, endOfInput,
                                          endOfLine, isEndOfLine, parseOnly,
                                          satisfy, skipSpace, string, takeTill,
                                          takeWhile1)
import           Data.Either.Combinators (fromRight')
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as H (fromList)
import           Data.Text               (Text)
import           Data.Text.IO            (readFile)
import           Data.Vector             (Vector)
import qualified Data.Vector             as V (fromList)

data Syns = Syns
  { word :: Text
  , syns :: Vector Text
  }

type Thes = HashMap Text (Vector Text)

moby :: Parser Thes
moby = do
  ts <- many thes
  endOfInput
  return . H.fromList $ (word &&& syns) <$> ts

thes :: Parser Syns
thes = do
  r <- root
  s <- V.fromList <$> many syn
  skipSpace
  return $ Syns r s

root :: Parser Text
root = do
  _ <- many digit >> string " Moby Thesaurus words for \""
  sr <- takeTill (== '"')
  char '"' >> char ':' >> skipSpace
  return sr

syn :: Parser Text
syn = do
  s <- takeWhile1 ((&&) <$> (/= ',') <*> not . isEndOfLine)
  _ <- interSyn
  return s

interSyn :: Parser Char
interSyn = (char ',' >> char ' ') <|>
           (char ',' >> endOfLine >> char ' ' >> char ' ' >> char ' ') <|>
           satisfy isEndOfLine

loadThes :: FilePath -> IO Thes
loadThes = (fromRight' . parseOnly moby <$>) . readFile
