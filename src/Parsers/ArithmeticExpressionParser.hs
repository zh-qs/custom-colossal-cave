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

-- |Match an expression of type @player.<parameter name>@.
parameterAccessorParser :: Parser Name
parameterAccessorParser = baseCodeLineParser "player" ((char '.' *> (unpack <$> takeWhile1 isAlphaNum) <* skipSpaces) <?> "parameter") "player"

-- |Match a number.
constantParser :: Parser (StIO Int)
constantParser = (lift . return) <$> (signed decimal)

-- |Match either a number or a parameter expression.
identificatorParser :: Parser (StIO Int)
identificatorParser = getPlayerParameter <$> parameterAccessorParser

-- |Match multiplication or division operator.
mulOpParser :: Parser (StIO Int -> StIO Int -> StIO Int)
mulOpParser = (charWithSpaces '*' *> liftPStIO (*)) -- <|> (charWithSpaces '/' *> return (\stx sty -> (/) <$> stx <*> sty))

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
factorParser = constantParser <|> identificatorParser <|> (charWithSpaces '(' *> expressionParser <* charWithSpaces ')')
