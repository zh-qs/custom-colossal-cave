{-# LANGUAGE OverloadedStrings #-}

module Parsers.CommandParser where

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
import Parsers.CodeParser

commandParser :: Parser (StIO ()) -> Parser Command
commandParser parser = 
    ((,) 
        <$> (unpack <$> takeWhile1 isAlphaNum) 
        <*> (char ':' *> skipSpaces *> parser <* newLines)) 
    <?> "Command definition"

commandListParser :: Int -> Parser (StIO ()) -> Parser [Command]
commandListParser indentationLevel parser = listParser "commands" (commandParser parser) indentationLevel "Command list"