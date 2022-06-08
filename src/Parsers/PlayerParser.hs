{-# LANGUAGE OverloadedStrings #-}

module Parsers.PlayerParser where

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
import Parsers.ItemParser
import Parsers.ParametersParser

inventoryParser :: StParser Inventory
inventoryParser = lift tabOrTwoSpaces *> itemListParser "inventory" 2 "Inventory definition" <* lift newLines

playerParser :: StParser Player
playerParser = (lift (string "player:\n") 
    *> (Player 
        <$> lift (parametersParser 1)
        <*> inventoryParser))
    <??> "Player definition"
