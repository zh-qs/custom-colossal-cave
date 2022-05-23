{-# LANGUAGE OverloadedStrings #-}

module Parsers.RoomParser where

import DataStructures
import qualified Data.Map.Strict as M
import Data.Attoparsec.Text
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Text
import Data.Char
import Data.List
import Commands
import Control.Applicative
import Data.Maybe
import Parsers.Utilities
import Parsers.ItemParser
import Parsers.CommandParser
import Parsers.CodeParser

descriptionParser :: Parser Desc
descriptionParser = multilineContentParser "description" 3 "Description definition"

roomParser :: StParser (Name,Room)
roomParser = 
    ((,)
        <$> lift ((unpack <$> takeWhile1 isAlphaNum) <* char ':' <* newLines) 
        <*> (Room 
            <$> lift (((tabs 3) *> descriptionParser <* newLines) <?> "Description")
            <*> ((lift (tabs 3) *> itemListParser "items" 4 "Item list definition" <* lift newLines) <??> "Item List")
            <*> lift (((tabs 3) *> commandListParser 4 codeParser <* newLines) <?> "Command List")))
    <??> "Room definition"

roomListParser :: StParser [(Name,Room)]
roomListParser = listParserSt "rooms" roomParser 1 "Room list definition"

testDescriptionParser :: Result Desc
testDescriptionParser = feed (parse (descriptionParser <* endOfInput) "description:\n        AAAAAAAAA\n        BBBBBBBB\n") ""

testRoomParser :: Result (Name,Room)
testRoomParser = feed 
    (parse (evalStateT roomParser M.empty <* endOfInput)
        "kitchen:\n\
        \      description:\n\
        \        You are in your kitchen, looking at the table.\n\
        \      items:\n\
        \        - food:\n\
        \            commands:\n\
        \        - drink:\n\
        \            commands:\n\
        \              - drink:\n\
        \                  {\n\
        
        \                  }\n\
        \      commands:\n\
        \        - nop:\n\
        \            {\n\
        \              print You just wait and do nothing.\n\
        \              goto kitchen\n\
        \            }\n"
    ) ""

 --   \              if player.hunger < 99 then player.hunger += 2 else player.hunger = 100\n\
 --       \              discard\n\