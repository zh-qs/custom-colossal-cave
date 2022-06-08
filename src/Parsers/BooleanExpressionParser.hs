{-# LANGUAGE OverloadedStrings #-}

module Parsers.BooleanExpressionParser where

import DataStructures
import qualified Data.Map.Strict as M
import Data.Attoparsec.Text
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import qualified Data.Text as T
import Data.Char
import Commands
import Control.Applicative
import Data.Maybe
import Parsers.Utilities
import Parsers.ArithmeticExpressionParser

-- |Match an @askYesNo@ prompt.
askYesNoParser :: Parser (Action Bool)
askYesNoParser = stringWithSpaces "askYesNo" *> pure (perform $ lift getLine >>= (\s -> if (toLower <$> s) `elem` ["y", "yes"] then return True else return False))

-- |Match a @prompt <word>@ prompt.
promptParser :: Parser (Action Bool)
promptParser = stringWithSpaces "prompt " *> (pure (\s -> perform $ lift $ (==s) <$> getLine) <*> (T.unpack <$> takeWhile1 isAlphaNum))

-- |Match a @has <item>@ test.
hasItemParser :: Parser (Action Bool)
hasItemParser = stringWithSpaces "has " *> (checkIfItemIsInInventory . T.unpack <$> takeWhile1 isAlphaNum)

-- |Match a @present <item|entity>@ test.
presentParser :: Parser (Action Bool)
presentParser = stringWithSpaces "present " *> (checkIfInteractablePresent . T.unpack <$> takeWhile1 isAlphaNum)

-- |Match a @present <item|entity> in <room>@ test.
presentInRoomParser :: Parser (Action Bool)
presentInRoomParser = stringWithSpaces "present " *> (checkIfInteractablePresentInRoom <$> (T.unpack <$> takeWhile1 isAlphaNum) <*> (stringWithSpaces "in " *> (T.unpack <$> takeWhile1 isAlphaNum)))

-- |Match an @in <room>@ test.
inRoomParser :: Parser (Action Bool)
inRoomParser = stringWithSpaces "in " *> (checkIfRoomIsCurrent . T.unpack <$> takeWhile1 isAlphaNum)

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
    <|> presentInRoomParser
    <|> inRoomParser
    <|> presentParser 
    <|> trueParser 
    <|> falseParser 
    <|> (expressionParser <**> comparisonOpParser <*> expressionParser) 
    <|> (charWithSpaces '(' *> booleanExpressionParser <* charWithSpaces ')')
    <|> (charWithSpaces '!' *> ((pure not <*>) <$> boolTermParser))
    <|> askYesNoParser
    <|> promptParser

-- |Match a boolean expression. 
booleanExpressionParser :: Parser (Action Bool)
booleanExpressionParser = (boolTermParser <**> boolOpParser <*> booleanExpressionParser) <|> boolTermParser