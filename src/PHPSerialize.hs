{-# LANGUAGE OverloadedStrings #-}
module PHPSerialize (
    PHPVal(..)
  , serialize
  , unserialize
  ) where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text as T


data PHPVal = PHPNull
            | PHPBool Bool
            | PHPInt Int
            | PHPDouble Double
            | PHPString T.Text
            | PHPArr [(PHPVal, PHPVal)]  
            deriving (Eq, Ord, Show)


pNull :: Parser PHPVal
pNull = PHPNull <$ (string "N;")


pBool :: Parser PHPVal
pBool = PHPBool <$ (string "b:1;") <*> pure True <|> PHPBool <$ (string "b:0;") <*> pure False


pInt :: Parser PHPVal
pInt = PHPInt <$ (string "i:") <*> decimal <* char ';'


pDouble :: Parser PHPVal
pDouble = PHPDouble <$ (string "d:") <*> double <* char ';'


pString :: Parser PHPVal
pString = do
  string "s:"
  len <- decimal
  string ":\""
  str <- Data.Attoparsec.Text.take len
  string "\";"
  return $ PHPString str


pKey :: Parser PHPVal
pKey = pInt <|> pString


pArr :: Parser PHPVal
pArr = do
  string "a:"
  len <- decimal
  string ":{"
  arr <- pArr' len (PHPArr [])
  char '}'
  return arr
  where pArr' :: Int -> PHPVal -> Parser PHPVal
        pArr' n (PHPArr xs)
          | n > 0     = do key <- pKey
                           val <- pVal
                           pArr' (n - 1) (PHPArr ((key, val) : xs))
          | otherwise = return $ PHPArr $ reverse xs
        pArr' _ _ = return $ PHPArr []


pVal :: Parser PHPVal
pVal = pNull <|> pBool <|> pInt <|> pDouble <|> pString <|> pArr


unserialize :: T.Text -> Maybe PHPVal
unserialize s = case parse pVal s of
                  Done "" r -> Just r
                  _         -> Nothing


serialize :: PHPVal -> T.Text
serialize PHPNull         = "N;"
serialize (PHPBool True)  = "b:1;"
serialize (PHPBool False) = "b:0;"
serialize (PHPInt n)      = T.concat ["i:", T.pack $ show n, ";"]
serialize (PHPDouble d)   = T.concat ["d:", T.pack $ show d, ";"]
serialize (PHPString s)   = T.concat ["s:", T.pack $ show $ T.length s, ":\"", s, "\";"]
serialize (PHPArr a)      = T.concat ["a:", T.pack $ show $ length a, ":{", arr, "}"]
  where arr = foldl (\s (k, v) -> T.concat [s, serialize k, serialize v]) "" a




