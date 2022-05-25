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

handParser :: Text -> StParser Hand
handParser keyword = 
    (lift ((tabOrTwoSpaces *> string keyword *> char ':' *> newLines) <?> (unpack keyword ++ " header"))
    *> lift (tabs 2)
    *> (
        lift (string "empty" *> newLines *> return Nothing)
        <|> (Just <$> itemParser 1)
    )) <??> (unpack keyword ++ " definition")

playerParser :: StParser Player
playerParser = (lift (string "player:\n") 
    *> (Player 
        <$> lift (parametersParser 2)
        <*> inventoryParser
        <*> handParser "leftHand" 
        <*> handParser "rightHand")) 
    <??> "Player definition"

testPlayerParser :: Result Player
testPlayerParser = feed 
    (parse (evalStateT playerParser M.empty <* endOfInput) 
        "player:\n\
        \  parameters:\n\
        \    - health:\n\
        \        value: 69\n\
        \    - hunger:\n\
        \        value: 101\n\
        \  inventory:\n\
        \    - axe:\n\
        \        commands:\n\
        \          - kill: { \n\
        \                    print Kill!\n\
        \                  }\n\
        \          - throw: {\n\
        \                     print Throw!\n\
        \                   }\n\
        \  rightHand:\n\
        \    potion:\n\
        \      commands:\n\
        \  leftHand:\n\
        \    empty\n"
    ) ""

