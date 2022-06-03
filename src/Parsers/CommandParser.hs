{-# LANGUAGE OverloadedStrings #-}

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
commandParser :: Parser (StIO ()) -> Parser [Command]
commandParser parser = 
    (\(b,ns,r) -> map (\n -> (b,n,r)) ns)
        <$> ((,,) 
            <$> ((char '*' *> pure False) <|> pure True)
            <*> ((T.unpack <$> takeWhile1 isAlphaNum) `sepBy` (char ',')) 
            <*> (char ':' *> skipSpaces *> parser <* newLines)) 
    <?> "Command definition"

-- |Match an item-specific command name with a code block.
itemCommandParser :: Parser (a -> StIO ()) -> Parser (a -> [Command])
itemCommandParser parser = 
    (\(b,ns,f) -> (\s -> map (\n -> (b,n,s)) ns) . f)
        <$> ((,,) 
            <$> ((char '*' *> pure False) <|> pure True)
            <*> ((T.unpack <$> takeWhile1 isAlphaNum) `sepBy` (char ',')) 
            <*> (char ':' *> skipSpaces *> parser <* newLines)) 
    <?> "Command definition"

baseItemCommandListParser :: T.Text -> Int -> Parser (a -> StIO ()) -> Parser (a -> [Command])
baseItemCommandListParser keyword indentationLevel parser = foldl' <$> pure (\f g -> (\n -> f n ++ g n)) <*> pure (const []) <*> listParser keyword (itemCommandParser parser) indentationLevel "Command list"

-- |Match a command list with custom keyword.
baseCommandListParser :: T.Text -> Int -> Parser (StIO ()) -> Parser [Command]
baseCommandListParser keyword indentationLevel parser = concat <$> listParser keyword (commandParser parser) indentationLevel "Command list"

-- |Match a command list.
commandListParser :: Int -> Parser (StIO ()) -> Parser [Command]
commandListParser = baseCommandListParser "commands"

testCommandParser :: Result ()
testCommandParser = void $ feed (parse (itemCommandParser itemCodeParser) "eat:\n\
    \            {\n\
    \              println Delicious!\n\
    \              player.hunger = player.hunger + 10\n\
    \              discard\n\
    \            }\n") ""