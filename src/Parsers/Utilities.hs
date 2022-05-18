{-# LANGUAGE OverloadedStrings #-}

module Parsers.Utilities where

import DataStructures
import qualified Data.Map.Strict as M
import Data.Attoparsec.Text
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Text
import Data.Char
import Commands
import Control.Applicative
import Data.Maybe
import Data.List

isNewline :: Char -> Bool
isNewline c = c == '\n' || c == '\r'

skipSpaces :: Parser ()
skipSpaces = skipWhile isSpace

charWithSpaces :: Char -> Parser Char
charWithSpaces c = skipSpaces *> char c <* skipSpaces

stringWithSpaces :: Text -> Parser Text
stringWithSpaces s = skipSpaces *> string s <* skipSpaces

newLines :: Parser ()
newLines = (skipWhile isNewline) <?> "no newline"

newLines1 :: Parser()
newLines1 = (skip isNewline *> skipWhile isNewline) <?> "no newline"

tabOrTwoSpaces :: Parser ()
tabOrTwoSpaces = ((void $ char '\t') <|> (void $ string "  ")) <?> "unproper indenting"

tabs :: Int -> Parser ()
tabs n = Prelude.foldr (*>) (return ()) (Prelude.take n $ repeat $ tabOrTwoSpaces)

listParser :: Text -> Parser a -> Int -> String -> Parser [a]
listParser keyword parser indentationLevel msg = 
    (((string keyword *> char ':') <?> "keyword") 
    *> newLines  
    *> (many' (tabs indentationLevel *> string "- " *> parser)))
    <?> msg

baseCodeLineParser :: Text -> Parser a -> String -> Parser a
baseCodeLineParser cmd parser msg = ((string cmd <?> "keyword") *> parser <* newLines) <?> msg 

multilineContentParser :: Text -> Int -> String -> Parser String
multilineContentParser keyword indentationLevel msg = (string keyword *> char ':' *> newLines *> (Data.List.foldl' <$> return (++) <*> return "" <*> many' (tabs (indentationLevel + 1) *> (((++"\n") . unpack) <$> Data.Attoparsec.Text.takeTill isNewline) <* newLines))) <?> msg

liftPStIO :: (a -> a -> b) -> Parser (StIO a -> StIO a -> StIO b)
liftPStIO f = return (\stx sty -> f <$> stx <*> sty)