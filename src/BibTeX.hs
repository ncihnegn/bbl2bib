module BibTeX (Entry (..), EntryType (..), fromEntry) where

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

fromEntry :: Entry -> String
fromEntry (Entry k (Publication t) ts) = "@" ++ t ++ "{" ++ k ++ ",\n" ++ unlines (mapMaybe fromTag ts) ++ "}"
fromEntry _ = ""

fromTag :: (String, String) -> Maybe String
fromTag ("labelalpha", _) = Nothing
fromTag ("labelnamesource", _) = Nothing
fromTag ("labeltitlesource", _) = Nothing
fromTag ("labelyear", _) = Nothing
fromTag ("sortinit", _) = Nothing
fromTag ("sortinithash", _) = Nothing
fromTag (n, v) = Just (n ++ " = " ++ v ++ ",")
