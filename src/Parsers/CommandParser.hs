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

-- |Match a command list.
commandListParser :: Int -> Parser (StIO ()) -> Parser [Command]
commandListParser indentationLevel parser = concat <$> listParser "commands" (commandParser parser) indentationLevel "Command list"