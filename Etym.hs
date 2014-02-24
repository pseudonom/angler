{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Etym where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Arrow           ((&&&))
import           Data.Maybe              (fromJust, isNothing)
import           Data.Monoid             (mempty)
import           GHC.Generics            (Generic)
import           Prelude                 hiding (any, elem, lookup, readFile,
                                          reverse)

import           Data.ByteString.Lazy    (readFile)
import           Data.Csv                (FromRecord, HasHeader (..), ToRecord,
                                          decode, parseRecord, record, toField,
                                          toRecord, (.!))
import           Data.Either.Combinators (fromRight')
import           Data.Hashable           (Hashable)
import           Data.HashMap.Strict     (HashMap, lookup)
import qualified Data.HashMap.Strict     as H (fromList)
import           Data.Text               (Text)
import           Data.Vector             (Vector, any, cons, elem, reverse)
import qualified Data.Vector             as V (toList)

data Etym = Etym
  { target :: Target
  , origin :: Origin
  } deriving (Eq, Ord, Show)
newtype Target = Target
  { getTarget :: Word
  } deriving (Eq, Show, Hashable, Ord)
newtype Lang = Lang
  { getLang :: Text
  } deriving (Eq, Show, Hashable, Ord)
newtype Origin = Origin
  { getOrigin :: Word
  } deriving (Eq, Show, Hashable, Ord)
data Word = Word
  { lang :: Lang
  , word :: Text
  } deriving (Generic, Eq, Show, Ord)
instance Hashable Word
instance FromRecord Etym where
  parseRecord v = Etym <$> ((Target .) . Word <$> (Lang <$> v .! 0) <*> v .! 1) <*>
                           ((Origin .) . Word <$> (Lang <$> v .! 2) <*> v .! 3)
instance ToRecord Etym where
  toRecord (Etym (Target (Word (Lang tl) tw ))
                 (Origin (Word (Lang ol) ow))) = record [ toField tl
                                                        , toField tw
                                                        , toField ol
                                                        , toField ow]

loadEtyms :: FilePath -> IO (HashMap Target Origin)
loadEtyms = (toHash . fromRight' <$>) . loadVec

rooted :: HashMap Target Origin -> Lang -> Target -> Bool
rooted h l = any ((l ==) . lang . getOrigin . origin) . terminalTrail h

toHash :: Vector Etym -> HashMap Target Origin
toHash = H.fromList . V.toList . ((target &&& origin) <$>)

terminalTrail :: HashMap Target Origin -> Target -> Vector Etym
terminalTrail h t' = go t' mempty where
  go t acc | isNothing o ||
             ((`elem` acc) . Etym t . fromJust $ o) = reverse acc
           | otherwise = go . Target . getOrigin <*> (`cons` acc) . Etym t $
                         fromJust o where
    o = lookup t h

loadVec :: FilePath -> IO (Either String (Vector Etym))
loadVec = (decode NoHeader <$>) .  readFile
