{-# LANGUAGE OverloadedStrings #-}

-- |Provides 'itemParser' and 'itemListParser', which are used to parse single item and item list.
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
import Parsers.ParametersParser

-- |Takes indentation level as an argument and returns parser of 'longName' property.
longNameParser :: Int -> Parser Name
longNameParser indentationLevel = (tabs indentationLevel *> string "longName: " *> (unpack <$> takeTill isNewline) <* newLines) <?> "Start room name"

-- |Second stage of 'itemParser'. Checks if there already exists an item in state of 'StParser'. If yes, then expects only name of an item (which was read in 'itemParser').
-- Else, expects whole definition of an item and after parsing it adds it to the map if the state.
modifyItemMapIfNeeded :: Int -> ItemName -> StParser ItemName
modifyItemMapIfNeeded indentationLevel name = 
    gets ((M.member name) . fst) 
    >>= (\exists -> if exists
        then lift (pure name <* newLines)
        else lift (Item 
                <$> (char ':' *> newLines *> longNameParser (indentationLevel + 2) <* newLines) 
                <*> (tabs (indentationLevel + 2) *> switchParser "description" (indentationLevel + 2) "Item description")
                <*> (parametersParser (indentationLevel + 2)) 
                <*> (tabs (indentationLevel + 2) *> commandListParser (indentationLevel + 3) (itemCodeParser <*> pure name) <* newLines))
            >>= (\item -> modify' (\(m,a) -> (M.insert name item m,a)))
            >> (lift $ pure name))

-- |Matches an item definition (or only item name if repeated)
itemParser :: Int -> StParser ItemName
itemParser indentationLevel = 
    (lift (unpack <$> takeWhile1 isAlphaNum) >>= modifyItemMapIfNeeded indentationLevel)
    <??> "Item definition"

-- |Matches an item list, which is declared with a keword 'Text', at indentation level 'Int'. In case of failure, 'String' is a message.
itemListParser :: Text -> Int -> String -> StParser [ItemName]
itemListParser keyword indentationLevel msg = listParserSt keyword (itemParser indentationLevel) indentationLevel msg