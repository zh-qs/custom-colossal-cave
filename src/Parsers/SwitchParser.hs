{-# LANGUAGE OverloadedStrings #-}

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

conditionPairParser :: Int -> Parser (Action Bool, String)
conditionPairParser indentationLevel = (,) <$> booleanExpressionParser <*> (char ':' *> newLines *> multilineContentParser' indentationLevel "multiline text")

conditionPairListParser :: Text -> Int -> String -> Parser [(Action Bool, String)]
conditionPairListParser keyword indentationLevel msg = listParser keyword (conditionPairParser (indentationLevel + 2)) (indentationLevel + 1) msg

switchParser :: Text -> Int -> String -> Parser (Action String)
switchParser keyword indentationLevel msg = Data.List.foldr (\(sbool,str) sstr -> sbool >>= (\b -> if b then return str else sstr)) (pure "") <$> conditionPairListParser keyword indentationLevel msg

testSwitchParser :: Result [String]
testSwitchParser = Data.List.map (\(sbool,str) -> str) <$> feed (parse (conditionPairListParser "description" 3 "AAA")
        "      description:\n\
        \        - player.life > 10:\n\
        \            Hej!!!\n\
        \            Ciesz sie zyciem!!!\n\
        \        - true:\n\
        \            Dzien dobry.\n\
        \            Czas mija.\n") ""