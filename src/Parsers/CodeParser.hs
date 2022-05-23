{-# LANGUAGE OverloadedStrings #-}

module Parsers.CodeParser where

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
import Parsers.BooleanExpressionParser

openCurlyBrace :: Parser Char
openCurlyBrace = char '{' <?> "Expecting '{' at the beginning of code block"

closeCurlyBrace :: Parser Char
closeCurlyBrace = char '}' <?> "Expecting '}' at the end of code block"

printEmptyLineParser :: Parser (StIO ())
printEmptyLineParser = baseCodeLineParser "print." (pure $ printMessage "\n") "print empty line"

printParser :: Parser (StIO ())
printParser = baseCodeLineParser "print " (printMessage . unpack <$> takeTill isNewline) "print"

printLineParser :: Parser (StIO ())
printLineParser = baseCodeLineParser "println " (printMessageLine . unpack <$> takeTill isNewline) "println"

printValueParser :: Parser (StIO ())
printValueParser = baseCodeLineParser "printval " ((>>= printInt) <$> expressionParser) "printval"

gotoParser :: Parser (StIO ())
gotoParser = baseCodeLineParser "goto " (goToRoom . unpack <$> takeTill isSpace) "goto"

assignmentParser :: Parser (StIO (Int -> Int))
assignmentParser = charWithSpaces '=' *> ((\sti -> return (\num _ -> num) <*> sti) <$> expressionParser)

parameterFunctionParser :: Parser (StIO (Int -> Int))
parameterFunctionParser = assignmentParser -- <|> modificationParser

changePlayerParameterParser :: Parser (StIO ())
changePlayerParameterParser = (flip (>>=) <$> (changePlayerParameter <$> parameterAccessorParser)) <*> parameterFunctionParser

conditionalParser :: Parser (StIO ())
conditionalParser = 
    (
        baseCodeLineParser "if " (conditionallyPerformAction <$> booleanExpressionParser <*> (stringWithSpaces "then" *> codeParser) <*> ((stringWithSpaces "else" *> codeParser) <|> pure noAction)) "conditional"
    )
    <?> "Conditional definition"


takeItemParser :: ItemName -> Parser (StIO ())
takeItemParser item = baseCodeLineParser "take" (pure $ takeItem item) "take"

dropItemParser :: ItemName -> Parser (StIO ())
dropItemParser item = baseCodeLineParser "drop" (pure $ dropItem item) "drop"

discardItemParser :: ItemName -> Parser (StIO ())
discardItemParser item = baseCodeLineParser "discard" (pure $ discardItem item) "discard"

codeLineParser :: Parser (StIO ())
codeLineParser = 
    (skipSpaces *> (
        gotoParser 
        <|> printEmptyLineParser
        <|> printParser
        <|> printLineParser
        <|> printValueParser
        <|> changePlayerParameterParser
        <|> conditionalParser
    )) 
    <?> "Command definition"

baseCodeParser :: Parser (StIO ()) -> Parser (StIO ())
baseCodeParser parser = openCurlyBrace *> (Prelude.foldr <$> pure (>>) <*> pure noAction <*> many' parser) <* skipSpaces <* closeCurlyBrace

codeParser :: Parser (StIO ())
codeParser = baseCodeParser codeLineParser

itemCodeLineParser :: ItemName -> Parser (StIO ())
itemCodeLineParser item = 
    (skipSpaces *> (
        codeLineParser
        <|> takeItemParser item
        <|> dropItemParser item
        <|> discardItemParser item
    ))
    <?> "Item command definition"

itemCodeParser :: ItemName -> Parser (StIO ())
itemCodeParser item = baseCodeParser $ itemCodeLineParser item