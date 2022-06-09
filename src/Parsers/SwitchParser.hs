{-# LANGUAGE OverloadedStrings #-}

-- |Provides 'switchParser', which is used to parse an object which returns different (multiline) text depending on specified conditions, 
-- that is, for a list of conditions, it returns the first text for which the condition is true. If no condition is satisfied,
-- returns empty string.
module Parsers.SwitchParser where

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
import Data.List
import Parsers.BooleanExpressionParser
import Parsers.Utilities

-- |Parse a single condition with 'booleanExpressionParser' and multiline text associated with it, at specified indentation level. 
conditionPairParser :: Int -> Parser (Action Bool, String)
conditionPairParser indentationLevel = (,) <$> booleanExpressionParser <*> (char ':' *> newLines *> multilineContentParser' indentationLevel "multiline text")

-- |Parse a list of conditions and associated texts, at specified indentation level.
conditionPairListParser :: Text -> Int -> String -> Parser [(Action Bool, String)]
conditionPairListParser keyword indentationLevel msg = listParser keyword (conditionPairParser (indentationLevel + 2)) (indentationLevel + 1) msg

-- |Parse an entire switch, where 'Text' is a keyword, 'Int' is an indentation level and 'String' is a message in case of parse failure.
switchParser :: Text -> Int -> String -> Parser (Action String)
switchParser keyword indentationLevel msg = Data.List.foldr (\(sbool,str) sstr -> sbool >>= (\b -> if b then return str else sstr)) (pure "") <$> conditionPairListParser keyword indentationLevel msg