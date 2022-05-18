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
--import GameParser

parameterAccessorParser :: Parser Name
parameterAccessorParser = baseCodeLineParser "player" ((char '.' *> (unpack <$> takeWhile1 isAlphaNum) <* skipSpaces) <?> "parameter") "player"

constantParser :: Parser (StIO Int)
constantParser = (lift . return) <$> (signed decimal)

identificatorParser :: Parser (StIO Int)
identificatorParser = getPlayerParameter <$> parameterAccessorParser

mulOpParser :: Parser (StIO Int -> StIO Int -> StIO Int)
mulOpParser = (charWithSpaces '*' *> liftPStIO (*)) -- <|> (charWithSpaces '/' *> return (\stx sty -> (/) <$> stx <*> sty))

addOpParser :: Parser (StIO Int -> StIO Int -> StIO Int)
addOpParser = (charWithSpaces '+' *> liftPStIO (+)) <|> (charWithSpaces '-' *> liftPStIO (-))

termParser :: Parser (StIO Int)
termParser = (factorParser <**> mulOpParser <*> termParser) <|> factorParser

expressionParser :: Parser (StIO Int)
expressionParser = (termParser <**> addOpParser <*> expressionParser) <|> termParser

factorParser :: Parser (StIO Int)
factorParser = constantParser <|> identificatorParser <|> (charWithSpaces '(' *> expressionParser <* charWithSpaces ')')

--testExpression :: String -> IO ()
--testExpression s = evalStateT (fromJust $ maybeResult $ feed (parse expressionParser $ pack s) "") (fromJust $ maybeResult $ testGameParser) >>= print