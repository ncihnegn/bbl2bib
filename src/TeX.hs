{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TeX (parseTeX, process) where

import BibTeX (Entry (Entry, entryType, key, tags), EntryType (Publication), fromEntry)
import Control.Applicative (many, (<|>))
import Control.Monad (void)
import Data.Char (isLetter, isSpace)
import Data.Foldable (asum)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Text.Parsec (try)
import Text.Parsec.Char (char, letter, newline, noneOf, oneOf, satisfy, spaces)
import Text.Parsec.Combinator (many1)
import Text.Parsec.String (Parser)

data TeXBlock
  = Comment String
  | Braced [TeXBlock]
  | Bracketed [TeXBlock]
  | Command String [TeXBlock]
  | Text String
  | Math String
  deriving (Eq, Show)

fromTeXBlock :: TeXBlock -> String
fromTeXBlock b = case b of
  Comment _ -> ""
  Braced bs -> "{" ++ fromTeXBlockList bs ++ "}"
  Bracketed bs -> "[" ++ fromTeXBlockList bs ++ "]"
  Command "bibnamedelima" _ -> "~"
  Command "bibnamedelimi" _ -> "~"
  Command "bibrangedash" _ -> "-"
  Command c bs -> "\\" ++ c ++ concatMap fromTeXBlock bs
  Text s -> s
  Math s -> "$" ++ s ++ "$"

fromTeXBlockList :: [TeXBlock] -> String
fromTeXBlockList = concatMap fromTeXBlock

unbraces :: String -> String
unbraces s
  | "{" `isPrefixOf` s && "}" `isSuffixOf` s = init $ tail s
  | otherwise = s

fromTeXArg :: TeXBlock -> String
fromTeXArg = unbraces . fromTeXBlock

specialChars :: String
specialChars = "&%$#_{}~^\\" ++ "!,>;:"

notText :: String
notText = "$%\\{}"

comment :: Parser TeXBlock
comment = do
  void $ char '%'
  line <- many $ noneOf "\n"
  spaces
  return $ Comment line

math :: Parser TeXBlock
math = do
  void $ char '$'
  s <- many $ noneOf "$"
  void $ char '$'
  return $ Math s

commandName :: Parser String
commandName =
  try spaceOnly
    <|> accent
    <|> escape
    <|> regularCommand
  where
    spaceOnly :: Parser String
    spaceOnly = do
      void $ many1 $ satisfy isSpace
      return " "
    accent :: Parser String
    accent = do
      s <- oneOf "`'^\"~=."
      l <- try letter <|> bracedLetter
      return [s, l]
      where
        bracedLetter :: Parser Char
        bracedLetter = do
          void $ char '{'
          ll <- letter
          void $ char '}'
          return ll
    escape = do
      c <- oneOf specialChars
      return [c]
    regularCommand :: Parser String
    regularCommand = many1 $ satisfy (\a -> isLetter a || a == '@')

command :: Parser TeXBlock
command = do
  void $ char '\\'
  name <- commandName
  args <- many $ try braced <|> spaceArg
  void $ many newline
  return $ Command name args
  where
    spaceArg :: Parser TeXBlock
    spaceArg = do
      void $ char ' '
      return $ Text " "

braced :: Parser TeXBlock
braced = do
  void $ char '{'
  blocks <- many block
  void $ char '}'
  return $ Braced blocks

bracketed :: Parser TeXBlock
bracketed = do
  void $ char '['
  blocks <- many block
  void $ char ']'
  return $ Bracketed blocks


text :: Parser TeXBlock
text = do
  s <- many1 $ noneOf notText
  spaces
  return $ Text s

block :: Parser TeXBlock
block =
  try comment
    <|> braced
    <|> command
    <|> text
    <|> math

parseTeX :: Parser [TeXBlock]
parseTeX = many block

comments :: [TeXBlock] -> [String]
comments =
  map (\case Comment s -> s; _ -> "") . filter isComment

biblatexVersion :: String -> Maybe (String, String)
biblatexVersion s
  | "biblatex" `isInfixOf` s && "version" `isInfixOf` s = Just ("biblatex", ws !! max 0 (length ws - 2))
  | otherwise = Nothing
  where
    ws = words s

version :: [String] -> Maybe (String, String)
version ss =
  asum $ map biblatexVersion ss

entries :: [TeXBlock] -> [[TeXBlock]]
entries bs =
  splitOn [Command "endentry" []] $ untilEntry bs

untilEntry :: [TeXBlock] -> [TeXBlock]
untilEntry = dropWhile (\case Command "entry" _ -> False; _ -> True)

entry2Bib :: [TeXBlock] -> Maybe Entry
entry2Bib [] = Nothing
entry2Bib (b : bs) = case entryHead b of
  Just (k, t) -> Just Entry {key = k, entryType = Publication t, tags = mapMaybe entryTag bs}
  Nothing -> Nothing
  where
    entryHead :: TeXBlock -> Maybe (String, String)
    entryHead b = case b of
      Command "entry" (k : t : _) -> Just (fromTeXArg k, fromTeXArg t)
      _ -> Nothing
    entryTag :: TeXBlock -> Maybe (String, String)
    entryTag b = case b of
      Command "name" [f, Braced _, _, Braced bss] -> Just (fromTeXArg f, authors bss)
      Command "list" [f, Braced _, v] -> Just (fromTeXArg f, fromTeXBlock v)
      Command "field" [k, v] -> Just (fromTeXArg k, fromTeXBlock v)
      Command "strng" [k, v] -> Nothing
      Command "range" [k, v] -> Nothing
      Command "verb" _ -> Nothing -- TODO
      Command "endverb" _ -> Nothing -- TODO
      Command "true" _ -> Nothing -- TODO
      Command "keyw" [ws] -> Just ("keywords", fromTeXBlock ws)
      Command c _ -> error ("Unknown command " ++ c)
      _ -> Nothing
      where
        authors :: [TeXBlock] -> String
        authors bss = "{" ++ intercalate " and " (map author $ filter (not . isComment) bss) ++ "}"

isComment :: TeXBlock -> Bool
isComment (Comment _) = True
isComment _ = False

isBraced :: TeXBlock -> Bool
isBraced (Braced _) = True
isBraced _ = False

author :: TeXBlock -> String
author (Braced bs) = realAuthor fs
  where
    fs = filter isBraced bs
author _ = "authorERROR"

realAuthor :: [TeXBlock] -> String
realAuthor [_, f, _, g, _, _, _, _, _] = intercalate ", " $ map fromTeXArg [f, g]
realAuthor [Braced _, Braced bs] = convert (filter (not . isComment) bs)
  where
    convert (_ : f : _ : _ : _ : g : _ : _ : xs) = intercalate ", " $ map fromTeXArg [f, g]
    convert _ = "convertERROR"
realAuthor _ = "realAuthorERROR"

process :: String -> [TeXBlock] -> IO ()
process file bs = do
  -- print bs
  -- print "Entries\n"
  -- print es
  print $ version $ comments bs
  writeFile file (unlines $ map fromEntry $ mapMaybe (entry2Bib . untilEntry) es)
  where
    es = entries bs
