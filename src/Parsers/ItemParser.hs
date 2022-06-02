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
import Parsers.CodeParser
import Parsers.SwitchParser

longNameParser :: Int -> Parser Name
longNameParser indentationLevel = (tabs indentationLevel *> string "longName: " *> (unpack <$> takeTill isNewline) <* newLines) <?> "Start room name"

modifyItemMapIfNeeded :: Int -> ItemName -> StParser ItemName
modifyItemMapIfNeeded indentationLevel name = 
    gets (M.member name) 
    >>= (\exists -> if exists
        then lift (pure name <* newLines)
        else lift (Item 
                <$> (char ':' *> newLines *> longNameParser (indentationLevel + 2) <* newLines) 
                <*> (tabs (indentationLevel + 2) *> switchParser "description" (indentationLevel + 2) "Item description")
                <*> (tabs (indentationLevel + 2) *> commandListParser (indentationLevel + 3) (itemCodeParser name) <* newLines))
            >>= (\item -> modify' (\m -> M.insert name item m))
            >> (lift $ pure name))

itemParser :: Int -> StParser ItemName
itemParser indentationLevel = 
    (lift (unpack <$> takeWhile1 isAlphaNum) >>= modifyItemMapIfNeeded indentationLevel)
    <??> "Item definition"

itemListParser :: Text -> Int -> String -> StParser [ItemName]
itemListParser keyword indentationLevel msg = listParserSt keyword (itemParser indentationLevel) indentationLevel msg

testItemParser :: Result ItemName
testItemParser = feed (parse (evalStateT (itemParser 1) M.empty <* endOfInput) "axe:\n      commands:\n        - kill: { print Kill!\n }\n") ""