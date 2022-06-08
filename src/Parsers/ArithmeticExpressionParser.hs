{-# LANGUAGE OverloadedStrings #-}

-- |Provides an 'expressionParser' parser for processing the arithmetic expressions.
-- The possible operations are: @+@, @-@, @*@, @/@ and @%@ (mod) and are defined for 'Int'.
-- Moreover, the parser provides accessors to parameters (@player.\<parameter\>@ and @entity.\<name\>.\<parameter\>@)
-- and expressions: @invcount@ (count of the player inventory), @cmdcount@ (number of entered commands during the game)
-- and @rnd@ (random number).
module Parsers.ArithmeticExpressionParser where

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
import System.Random


-- * Grammar
--
-- $grammar
--
-- The arithmetic expression grammar in this module is defined as follows:
--
-- * expr -> term expr'
-- * expr' -> (@+@|@-@) term expr' | eps
-- * term -> factor | term'
-- * term' -> (@*@|@/@|@%@) factor term' | eps
-- * factor -> @invcount@ | @cmdcount@ | @rnd@ | number | @player.\<parameter\>@ | @entity.\<name\>.parameter@ | ( expr )
--
-- All operators are left-associative. Operators @*,/,%@ have higher priority than @+@ and @-@.
--

-- * Parsing arithmetic expressions

-- |Match a @cmdcount@ expression.
commandCountParser :: Parser (Action Int)
commandCountParser = stringWithSpaces "cmdcount" *> pure getCommandCount

-- |Match an @invcount@ expression.
inventoryCountParser :: Parser (Action Int)
inventoryCountParser = stringWithSpaces "invcount" *> pure getInventoryCount

-- |Match a @rnd@ expression.
randomNumberParser :: Parser (Action Int)
randomNumberParser = stringWithSpaces "rnd" *> pure randomIO

-- |Match an expression of type @entity.<entity name>.<parameter name>@.
entityParameterAccessorParser :: Parser (Name,Name)
entityParameterAccessorParser = baseCodeLineParser "entity" ((char '.' *> ((,) <$> (unpack <$> takeWhile1 isAlphaNum) <*> (char '.' *> (unpack <$> takeWhile1 isAlphaNum)) <?> "parameter"))) "entity"

-- |Match an expression of type @player.<parameter name>@.
playerParameterAccessorParser :: Parser Name
playerParameterAccessorParser = baseCodeLineParser "player" ((char '.' *> (unpack <$> takeWhile1 isAlphaNum) <* skipSpaces) <?> "parameter") "player"

-- |Match a number.
constantParser :: Parser (Action Int)
constantParser = (perform . lift . return) <$> (signed decimal)

-- |Match a parameter accessor expression.
identificatorParser :: Parser (Action Int)
identificatorParser = (getPlayerParameter <$> playerParameterAccessorParser) <|> ((uncurry getEntityParameter) <$> entityParameterAccessorParser)

-- |Match multiplication or division operator.
mulOpParser :: Parser (Action Int -> Action Int -> Action Int)
mulOpParser = (charWithSpaces '*' *> liftPAction (*)) <|> (charWithSpaces '/' *> liftPAction (flip div)) <|> (charWithSpaces '%' *> liftPAction (flip mod))

-- |Match addition or subtraction operator.
addOpParser :: Parser (Action Int -> Action Int -> Action Int)
addOpParser = (charWithSpaces '+' *> liftPAction (+)) <|> (charWithSpaces '-' *> liftPAction (flip (-)))

-- |Match a single term of a sum.
termParser :: Parser (Action Int)
termParser = factorParser <**> termParser'

-- |Match a @term'@ non-terminal from a grammar, preserving left-associativity of the operators.
termParser' :: Parser (Action Int -> Action Int)
termParser' = ((flip (.) <$> (mulOpParser <*> factorParser)) <*> termParser') <|> pure id

-- |Match an arithmetic expression and return its symbolic representation in a parser.
expressionParser :: Parser (Action Int)
expressionParser = termParser <**> expressionParser'

-- |Match a @expr'@ non-terminal from a grammar, preserving left-associativity of the operators.
expressionParser' :: Parser (Action Int -> Action Int)
expressionParser' = ((flip (.) <$> (addOpParser <*> termParser)) <*> expressionParser') <|> pure id

-- |Match a single factor of a term.
factorParser :: Parser (Action Int)
factorParser = 
    inventoryCountParser 
    <|> commandCountParser
    <|> randomNumberParser 
    <|> constantParser 
    <|> identificatorParser 
    <|> (charWithSpaces '(' *> expressionParser <* charWithSpaces ')')
