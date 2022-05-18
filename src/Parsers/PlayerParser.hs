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

singleParameterParser :: Parser (Name, Int)
singleParameterParser = 
    ((tabs 2) 
    *> "- " 
    *> ((,) 
        <$> (unpack <$> takeWhile1 isAlphaNum) 
        <*> (char ':' *> newLines *> (tabs 4) *> string "value: " *> (decimal <?> "Parameter value") <* newLines))) 
    <?> "Single parameter"

parametersHeaderParser :: Parser Text
parametersHeaderParser = (tabOrTwoSpaces *> string "parameters:" <* newLines) <?> "Player parameters header"

parametersParser :: Parser (M.Map Name Int)
parametersParser = (parametersHeaderParser *> (M.fromList <$> many' singleParameterParser)) <?> "Player parameters definition"

inventoryParser :: Parser Inventory
inventoryParser = tabOrTwoSpaces *> itemListParser "inventory" 2 "Inventory definition" <* newLines

handParser :: Text -> Parser Hand
handParser keyword = 
    (((tabOrTwoSpaces *> string keyword *> char ':' *> newLines) <?> (unpack keyword ++ " header"))
    *> (tabs 2)
    *> (
        (string "empty" *> newLines *> return Nothing)
        <|> (Just <$> itemParser 1)
    )) <?> (unpack keyword ++ " definition")

playerParser :: Parser Player
playerParser = (string "player:\n" 
    *> (Player 
        <$> parametersParser 
        <*> inventoryParser
        <*> handParser "leftHand" 
        <*> handParser "rightHand")) 
    <?> "Player definition"

testPlayerParser :: Result Player
testPlayerParser = feed 
    (parse (playerParser <* endOfInput) 
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

