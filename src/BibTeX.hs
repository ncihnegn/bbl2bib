module BibTeX (Entry (..), EntryType (..), fromEntry, unbraces) where

import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (mapMaybe)

data EntryType
  = Abbreviation
  | Comment
  | Preamble
  | Publication String
  deriving (Eq, Show)

data Entry = Entry
  { key :: String,
    entryType :: EntryType,
    tags :: [(String, String)]
  }
  deriving (Eq, Show)

unbraces :: String -> String
unbraces s
  | "{" `isPrefixOf` s && "}" `isSuffixOf` s = init $ tail s
  | otherwise = s

fromEntry :: Entry -> String
fromEntry (Entry k (Publication t) ts) = "@" ++ t ++ "{" ++ k ++ ",\n" ++ unlines (mapMaybe fromTag ts) ++ "}"
fromEntry _ = ""

fromTag :: (String, String) -> Maybe String
fromTag ("labelyear", _) = Nothing
fromTag ("sortinit", _) = Nothing
fromTag (n, v) = Just (n ++ " = " ++ v ++ ",")
