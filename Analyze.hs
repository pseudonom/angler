{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Analyze where

import           Control.Applicative ((<$>))
import           Control.Monad       ((<=<))
import           Prelude             hiding (filter, getLine, head, lookup,
                                      null, putStrLn, words)

import           Control.Error.Util  (note)
import           Data.HashMap.Strict (HashMap, lookup, (!))
import qualified Data.HashMap.Strict as H (fromList)
import           Data.List.Ordered   (nubSort)
import           Data.Text           (Text)
import           Data.Text.ICU       (Break, breakWord, breaks, brkBreak,
                                      brkStatus, toLower)
import qualified Data.Text.ICU       as I (Word (..))
import           Data.Text.ICU.Types (LocaleName (..))
import           Data.Vector         (Vector, filter, null)
import qualified Data.Vector         as V (fromList, toList)
import           NLP.Snowball        (Algorithm (..), stem)

import           Etym                (Lang (..), Origin, Target (..), Word (..),
                                      rooted)
import           Thesaurus           (Thes)


data Fail = NoSyn
          | NoEtym
          | NotBad
          deriving Show

data TextInfo = WordLike { wordLike :: WordInfo }
              | NonWord  { nonWord :: Text      }
              deriving Show

data WordInfo = WordInfo
  { orig :: Text
  , info :: Either Fail (Vector Text)
  } deriving Show

locale :: Lang -> LocaleName
locale (Lang "Dan") = Locale "da"
locale (Lang "Nld") = Locale "nl"
locale (Lang "Eng") = Locale "en"
locale (Lang "Fin") = Locale "fi"
locale (Lang "Fra") = Locale "fr"
locale (Lang "Ger") = Locale "de"
locale (Lang "Hun") = Locale "hu"
locale (Lang "Ita") = Locale "it"
locale (Lang "Nor") = Locale "no"
locale (Lang "Por") = Locale "pt"
locale (Lang "Ron") = Locale "ro"
locale (Lang "Rus") = Locale "ru"
locale (Lang "Spa") = Locale "es"
locale (Lang "Swe") = Locale "sv"
locale (Lang "Tur") = Locale "tr"

algo :: Lang -> Algorithm
algo (Lang "Dan") = Danish
algo (Lang "Nld") = Dutch
algo (Lang "Eng") = English
algo (Lang "Fin") = Finnish
algo (Lang "Fra") = French
algo (Lang "Ger") = German
algo (Lang "Hun") = Hungarian
algo (Lang "Ita") = Italian
algo (Lang "Nor") = Norwegian
algo (Lang "Por") = Portuguese
algo (Lang "Ron") = Romanian
algo (Lang "Rus") = Russian
algo (Lang "Spa") = Spanish
algo (Lang "Swe") = Swedish
algo (Lang "Tur") = Turkish

analyze :: HashMap Target Origin -> Thes ->
           InLang -> BadLang -> GoodLang -> Text -> Vector TextInfo
analyze e s (InLang il) bl gl t = annotate (InLang il) h <$> ws where
  h = synSet e s (InLang il) bl gl . (brkBreak <$>) .
      filter ((== I.Letter) . brkStatus) $ ws
  ws = V.fromList . breaks (breakWord $ locale il) $ t

annotate :: InLang -> HashMap Text (Either Fail (Vector Text)) -> Break I.Word ->
            TextInfo
annotate (InLang il) h b
  | brkStatus b == I.Letter =
    WordLike . WordInfo (brkBreak b) $
    h ! toLower (locale il) (brkBreak b)
  | otherwise = NonWord . brkBreak $ b

newtype BadLang = BadLang { getBadLang :: Lang }
newtype GoodLang = GoodLang { getGoodLang :: Lang }
newtype InLang = InLang { getInLang :: Lang }

synSet :: HashMap Target Origin -> Thes ->
          InLang -> BadLang -> GoodLang ->
          Vector Text -> HashMap Text (Either Fail (Vector Text))
synSet e s (InLang il) bl'@(BadLang bl) gl ts  =
  H.fromList $ zip ts' (is <$> ts') where
    is w | rooted e bl . Target $ w' =
      either (const $ langSyns e s bl' gl w'') Right $ langSyns e s bl' gl w'
         | otherwise = Left NotBad where
           w' = Word il w
           w'' = Word il . stem (algo il) $ w
    ts' = nubSort . (toLower (locale il) <$>) . V.toList $ ts

-- Lang of Word ought to be same as language of thesaurus
langSyns :: HashMap Target Origin -> Thes -> BadLang ->
            GoodLang -> Word -> Either Fail (Vector Text)
langSyns e s (BadLang bl) (GoodLang gl) (Word il w) =
  origLang <=< note NoSyn . lookup w $ s where
    origLang ss | null ss' = Left NoEtym
                | otherwise = Right ss' where
                  ss' = filter (\(Target . Word il -> t) ->
                                rooted e gl t && not (rooted e bl t)) ss
