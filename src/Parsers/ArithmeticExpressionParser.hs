{-# LANGUAGE OverloadedStrings #-}

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

-- |Match a @rnd@ expression.
randomNumberParser :: Parser (StIO Int)
randomNumberParser = stringWithSpaces "rnd" *> pure randomIO

-- |Match an expression of type @entity.<entity name>.<parameter name>@.
entityParameterAccessorParser :: Parser (Name,Name)
entityParameterAccessorParser = baseCodeLineParser "entity" ((char '.' *> ((,) <$> (unpack <$> takeWhile1 isAlphaNum) <*> (char '.' *> (unpack <$> takeWhile1 isAlphaNum)) <?> "parameter"))) "entity"

-- |Match an expression of type @player.<parameter name>@.
playerParameterAccessorParser :: Parser Name
playerParameterAccessorParser = baseCodeLineParser "player" ((char '.' *> (unpack <$> takeWhile1 isAlphaNum) <* skipSpaces) <?> "parameter") "player"

-- |Match a number.
constantParser :: Parser (StIO Int)
constantParser = (lift . return) <$> (signed decimal)

-- |Match a parameter accessor expression.
identificatorParser :: Parser (StIO Int)
identificatorParser = (getPlayerParameter <$> playerParameterAccessorParser) <|> ((uncurry getEntityParameter) <$> entityParameterAccessorParser)

-- |Match multiplication or division operator.
mulOpParser :: Parser (StIO Int -> StIO Int -> StIO Int)
mulOpParser = (charWithSpaces '*' *> liftPStIO (*)) <|> (charWithSpaces '/' *> liftPStIO div) <|> (charWithSpaces '%' *> liftPStIO mod)

-- |Match addition or subtraction operator.
addOpParser :: Parser (StIO Int -> StIO Int -> StIO Int)
addOpParser = (charWithSpaces '+' *> liftPStIO (+)) <|> (charWithSpaces '-' *> liftPStIO (-))

-- |Match a single term of a sum.
termParser :: Parser (StIO Int)
termParser = (factorParser <**> mulOpParser <*> termParser) <|> factorParser

-- |Match an arithmetic expression and return its symbolic representation in a parser.
expressionParser :: Parser (StIO Int)
expressionParser = (termParser <**> addOpParser <*> expressionParser) <|> termParser

-- |Match a single factor of a term.
factorParser :: Parser (StIO Int)
factorParser = randomNumberParser <|> constantParser <|> identificatorParser <|> (charWithSpaces '(' *> expressionParser <* charWithSpaces ')')
