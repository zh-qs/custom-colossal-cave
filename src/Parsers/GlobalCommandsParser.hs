{-# LANGUAGE OverloadedStrings #-}

-- |Provide parsers to read global commands lists and 'onEntry' command.
module Parsers.GlobalCommandsParser where

import DataStructures
import qualified Data.Map.Strict as M
import Data.Attoparsec.Text
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import qualified Data.Text as T
import Data.Char
import Commands
import Control.Applicative
import Data.Maybe
import Parsers.Utilities
import Parsers.CodeParser
import Parsers.CommandParser

-- |Match a @global:@ header with newline characters.
globalHeaderParser :: StParser ()
globalHeaderParser = lift (string "global:" *> newLines <?> "Global functions header")

-- |Match a list of global item commands.
globalItemCommandsParser :: StParser (ItemName -> [Command])
globalItemCommandsParser = lift $ tabs 1 *> baseItemCommandListParser "item" 2 itemCodeParser

-- |Match a list of global room commands.
globalRoomCommandsParser :: StParser [Command]
globalRoomCommandsParser = lift $ tabs 1 *> baseCommandListParser "room" 2 codeParser

-- |Match a global room 'globalOnEntry' command.
globalOnEntryParser :: StParser (Action ())
globalOnEntryParser = lift (onEntryParser 1 noAction) >>= (\a  -> modify' (\(m,_) -> (m,a)) >> return a)