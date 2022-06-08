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
import Parsers.SwitchParser

descriptionParser :: Parser Desc
descriptionParser = switchParser "description" 3 "Description definition"

roomParser :: StParser (Name,Room)
roomParser = gets snd >>= (\action -> 
    ((,)
        <$> lift ((unpack <$> takeWhile1 isAlphaNum) <* char ':' <* newLines) 
        <*> (Room 
            <$> lift (((tabs 3) *> descriptionParser <* newLines) <?> "Description")
            <*> lift (onEntryParser 3 action)
            <*> ((lift (tabs 3) *> itemListParser "items" 4 "Item list definition" <* lift newLines) <??> "Item List")
            <*> lift (((tabs 3) *> commandListParser 4 codeParser <* newLines) <?> "Command List")))
    <??> "Room definition")

roomListParser :: StParser [(Name,Room)]
roomListParser = listParserSt "rooms" roomParser 1 "Room list definition"