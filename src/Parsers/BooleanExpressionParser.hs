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
--import GameParser

comparisonOpParser :: Parser (StIO Int -> StIO Int -> StIO Bool)
comparisonOpParser = 
    (stringWithSpaces "==" *> liftPStIO (==)) 
    <|> (stringWithSpaces ">=" *> liftPStIO (>=))
    <|> (stringWithSpaces "<=" *> liftPStIO (<=))
    <|> (charWithSpaces '>' *> liftPStIO (>))
    <|> (charWithSpaces '<' *> liftPStIO (<))


boolOpParser :: Parser (StIO Bool -> StIO Bool -> StIO Bool)
boolOpParser = (stringWithSpaces "&&" *> liftPStIO (&&)) <|> (stringWithSpaces "||" *> liftPStIO (||))

trueParser :: Parser (StIO Bool)
trueParser = (stringWithSpaces "true" <|> stringWithSpaces "True" <|> stringWithSpaces "TRUE") *> (pure . lift . return) True

falseParser :: Parser (StIO Bool)
falseParser = (stringWithSpaces "false" <|> stringWithSpaces "False" <|> stringWithSpaces "FALSE") *> (pure . lift . return) False

boolTermParser :: Parser (StIO Bool)
boolTermParser = trueParser <|> falseParser <|> (expressionParser <**> comparisonOpParser <*> expressionParser) <|> (charWithSpaces '(' *> booleanExpressionParser <* charWithSpaces ')')

booleanExpressionParser :: Parser (StIO Bool)
booleanExpressionParser = (boolTermParser <**> boolOpParser <*> booleanExpressionParser) <|> boolTermParser

--testExpression :: String -> IO ()
--testExpression s = evalStateT (fromJust $ maybeResult $ feed (parse booleanExpressionParser $ pack s) "") (fromJust $ maybeResult $ testGameParser) >>= print