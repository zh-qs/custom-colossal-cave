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

-- |Match a @has <item>@ test.
hasItemParser :: Parser (Action Bool)
hasItemParser = (stringWithSpaces "has" *> (checkIfItemIsInInventory . unpack <$> takeTill isSpace))

-- |Match a comparison operator.
comparisonOpParser :: Parser (Action Int -> Action Int -> Action Bool)
comparisonOpParser = 
    (stringWithSpaces "==" *> liftPAction (==)) 
    <|> (stringWithSpaces "!=" *> liftPAction (/=)) 
    <|> (stringWithSpaces ">=" *> liftPAction (>=))
    <|> (stringWithSpaces "<=" *> liftPAction (<=))
    <|> (charWithSpaces '>' *> liftPAction (>))
    <|> (charWithSpaces '<' *> liftPAction (<))

-- |Match either a boolean sum or product.
boolOpParser :: Parser (Action Bool -> Action Bool -> Action Bool)
boolOpParser = (stringWithSpaces "&&" *> liftPAction (&&)) <|> (stringWithSpaces "||" *> liftPAction (||))

-- |Match true value. The following representations are correct: @true@, @True@, @TRUE@.
trueParser :: Parser (Action Bool)
trueParser = (stringWithSpaces "true" <|> stringWithSpaces "True" <|> stringWithSpaces "TRUE") *> (pure . pure) True

-- |Match false value. The following representations are correct: @false@, @False@, @FALSE@.
falseParser :: Parser (Action Bool)
falseParser = (stringWithSpaces "false" <|> stringWithSpaces "False" <|> stringWithSpaces "FALSE") *> (pure . pure) False

-- |Match a sigle term of a boolean expression.
boolTermParser :: Parser (Action Bool)
boolTermParser = 
    hasItemParser 
    <|> trueParser 
    <|> falseParser 
    <|> (expressionParser <**> comparisonOpParser <*> expressionParser) 
    <|> (charWithSpaces '(' *> booleanExpressionParser <* charWithSpaces ')')
    <|> (charWithSpaces '!' *> ((pure not <*>) <$> boolTermParser))

-- |Match a boolean expression. 
booleanExpressionParser :: Parser (Action Bool)
booleanExpressionParser = (boolTermParser <**> boolOpParser <*> booleanExpressionParser) <|> boolTermParser