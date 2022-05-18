{-# LANGUAGE OverloadedStrings #-}

module Parsers.ItemParser where

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
import Parsers.Utilities
import Parsers.CommandParser

itemParser :: Int -> Parser Item
itemParser indentationLevel = 
    (Item 
        <$> (unpack <$> takeWhile1 isAlphaNum) 
        <*> (char ':' *> newLines *> tabs (indentationLevel + 2) *> commandListParser (indentationLevel + 3) <* newLines)) 
    <?> "Item definition"

itemListParser :: Text -> Int -> String -> Parser [Item]
itemListParser keyword indentationLevel msg = listParser keyword (itemParser indentationLevel) indentationLevel msg

testItemParser :: Result Item
testItemParser = feed (parse ((itemParser 1) <* endOfInput) "axe:\n      commands:\n        - kill: { print Kill!\n }\n") ""