{-# LANGUAGE OverloadedStrings #-}

module Parsers.BooleanExpressionParser where

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
import Parsers.ArithmeticExpressionParser

-- |Match a comparison operator.
comparisonOpParser :: Parser (StIO Int -> StIO Int -> StIO Bool)
comparisonOpParser = 
    (stringWithSpaces "==" *> liftPStIO (==)) 
    <|> (stringWithSpaces ">=" *> liftPStIO (>=))
    <|> (stringWithSpaces "<=" *> liftPStIO (<=))
    <|> (charWithSpaces '>' *> liftPStIO (>))
    <|> (charWithSpaces '<' *> liftPStIO (<))

-- |Match either a boolean sum or product.
boolOpParser :: Parser (StIO Bool -> StIO Bool -> StIO Bool)
boolOpParser = (stringWithSpaces "&&" *> liftPStIO (&&)) <|> (stringWithSpaces "||" *> liftPStIO (||))

-- |Match true value. The following representations are correct: @true@, @True@, @TRUE@.
trueParser :: Parser (StIO Bool)
trueParser = (stringWithSpaces "true" <|> stringWithSpaces "True" <|> stringWithSpaces "TRUE") *> (pure . lift . return) True

-- |Match false value. The following representations are correct: @false@, @False@, @FALSE@.
falseParser :: Parser (StIO Bool)
falseParser = (stringWithSpaces "false" <|> stringWithSpaces "False" <|> stringWithSpaces "FALSE") *> (pure . lift . return) False

-- |Match a sigle term of a boolean expression.
boolTermParser :: Parser (StIO Bool)
boolTermParser = trueParser <|> falseParser <|> (expressionParser <**> comparisonOpParser <*> expressionParser) <|> (charWithSpaces '(' *> booleanExpressionParser <* charWithSpaces ')')

-- |Match a boolean expression. 
booleanExpressionParser :: Parser (StIO Bool)
booleanExpressionParser = (boolTermParser <**> boolOpParser <*> booleanExpressionParser) <|> boolTermParser