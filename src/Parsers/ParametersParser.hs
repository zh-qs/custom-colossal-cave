{-# LANGUAGE OverloadedStrings #-}

-- |Provides 'parametersParser', which is used to parse 'playerParameters' and 'itemParameters' properties. All parameters are of type 'Int'.
module Parsers.ParametersParser where

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
import Parsers.CommandParser
import Parsers.CodeParser

-- |Match a single parameter, taking an indentation level as an argument.
singleParameterParser :: Int -> Parser (Name, Int)
singleParameterParser indentationLevel = 
    ((tabs indentationLevel) 
    *> "- " 
    *> ((,) 
        <$> (unpack <$> takeWhile1 isAlphaNum) 
        <*> (char ':' *> newLines *> (tabs (indentationLevel + 2)) *> string "value: " *> (decimal <?> "Parameter value") <* newLines))) 
    <?> "Single parameter"

-- |Match a @parameters:@ keyword, at specified indentation level.
parametersHeaderParser :: Int -> Parser Text
parametersHeaderParser indentationLevel = (tabs indentationLevel *> string "parameters:" <* newLines) <?> "Parameters header"

-- |Match a paramter list and return 'Map Name Int', at specified indentation level.
parametersParser :: Int -> Parser (M.Map Name Int)
parametersParser indentationLevel = (parametersHeaderParser indentationLevel *> (M.fromList <$> many' (singleParameterParser (indentationLevel + 1)))) <?> "Parameters definition"