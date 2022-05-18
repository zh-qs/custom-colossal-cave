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

commandParser :: Parser Command
commandParser = 
    ((,) 
        <$> (unpack <$> takeWhile1 isAlphaNum) 
        <*> (char ':' *> skipSpaces *> codeParser <* newLines)) 
    <?> "Command definition"

commandListParser :: Int -> Parser [Command]
commandListParser indentationLevel = listParser "commands" commandParser indentationLevel "Command list"