{-# LANGUAGE OverloadedStrings #-}

-- |Provides 'commandListParser', which parses YAML-like list of commands.
module Parsers.CommandParser where

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
import Data.List
import Parsers.Utilities
import Parsers.CodeParser

-- |Match a command name with a code block.
commandParser :: Parser (Action ()) -> Parser [Command]
commandParser parser = 
    (\(b,ns,r) -> map (\n -> (b,n,r)) ns)
        <$> ((,,) 
            <$> ((char '*' *> pure False) <|> pure True)
            <*> ((T.unpack <$> takeWhile1 isAlphaNum) `sepBy` (char ',')) 
            <*> (char ':' *> skipSpaces *> parser <* newLines)) 
    <?> "Command definition"

-- |Match an item-specific command name with a code block.
itemCommandParser :: Parser (a -> Action ()) -> Parser (a -> [Command])
itemCommandParser parser = 
    (\(b,ns,f) -> (\s -> map (\n -> (b,n,s)) ns) . f)
        <$> ((,,) 
            <$> ((char '*' *> pure False) <|> pure True)
            <*> ((T.unpack <$> takeWhile1 isAlphaNum) `sepBy` (char ',')) 
            <*> (char ':' *> skipSpaces *> parser <* newLines)) 
    <?> "Command definition"

-- |Match an item command list with custom keyword.
baseItemCommandListParser :: T.Text -> Int -> Parser (a -> Action ()) -> Parser (a -> [Command])
baseItemCommandListParser keyword indentationLevel parser = foldl' <$> pure (\f g -> (\n -> f n ++ g n)) <*> pure (const []) <*> listParser keyword (itemCommandParser parser) indentationLevel "Command list"

-- |Match a command list with custom keyword.
baseCommandListParser :: T.Text -> Int -> Parser (Action ()) -> Parser [Command]
baseCommandListParser keyword indentationLevel parser = concat <$> listParser keyword (commandParser parser) indentationLevel "Command list"

-- |Match a command list. Takes indentation level and either 'commandParser' or 'itemCommandParser' as an argument.
commandListParser :: Int -> Parser (Action ()) -> Parser [Command]
commandListParser = baseCommandListParser "commands"

-- |Match an 'onEntry' command.
onEntryParser :: Int -> Action () -> Parser (Action ())
onEntryParser indentationLevel defaultAction = (tabs indentationLevel *> string "onEntry:" *> newLines *> skipSpaces *> codeParser <* newLines) <|> pure defaultAction