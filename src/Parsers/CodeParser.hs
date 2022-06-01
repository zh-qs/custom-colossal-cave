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

-- TUTAJ MOŻE OPISAĆ KOD!!!

-- |Match an opening curly brace (@{@) of code block. 
openCurlyBrace :: Parser Char
openCurlyBrace = char '{' <?> "Expecting '{' at the beginning of code block"

-- |Match a closing curly brace (@}@) of code block. 
closeCurlyBrace :: Parser Char
closeCurlyBrace = char '}' <?> "Expecting '}' at the end of code block"

-- |Match a @print.@ instruction.
printEmptyLineParser :: Parser (StIO ())
printEmptyLineParser = baseCodeLineParser "print." (pure $ printMessage "\n") "print empty line"

-- |Match a @print <text>@ instruction.
printParser :: Parser (StIO ())
printParser = baseCodeLineParser "print " (printMessage . unpack <$> takeTill isNewline) "print"

-- |Match a @println <text>@ instruction.
printLineParser :: Parser (StIO ())
printLineParser = baseCodeLineParser "println " (printMessageLine . unpack <$> takeTill isNewline) "println"

-- |Match a @printval <expression>@ instruction.
printValueParser :: Parser (StIO ())
printValueParser = baseCodeLineParser "printval " ((>>= printInt) <$> expressionParser) "printval"

-- |Match a @goto <room>@ instruction.
gotoParser :: Parser (StIO ())
gotoParser = baseCodeLineParser "goto " (goToRoom . unpack <$> takeTill isSpace) "goto"

-- |Match an assignment instruction.
assignmentParser :: Parser (StIO (Int -> Int))
assignmentParser = charWithSpaces '=' *> ((\sti -> return (\num _ -> num) <*> sti) <$> expressionParser)

-- |Match a modification instruction.
modificationParser :: Parser (StIO (Int -> Int))
modificationParser = 
    stringWithSpaces "+=" *> ((\sti -> return (+) <*> sti) <$> expressionParser) 
    <|> stringWithSpaces "-=" *> ((\sti -> return (flip (-)) <*> sti) <$> expressionParser)
    <|> stringWithSpaces "*=" *> ((\sti -> return (*) <*> sti) <$> expressionParser) 

-- |Match either an assignment or a modification instruction.
parameterFunctionParser :: Parser (StIO (Int -> Int))
parameterFunctionParser = assignmentParser <|> modificationParser

-- |Match an instruction of type @player.<parameter> =/+=/-=/*= <expression>@.
changePlayerParameterParser :: Parser (StIO ())
changePlayerParameterParser = (flip (>>=) <$> (changePlayerParameter <$> playerParameterAccessorParser)) <*> parameterFunctionParser

-- |Match an instruction of type @entity.<entity name>.<parameter> =/+=/-=/*= <expression>@.
changeEntityParameterParser :: Parser (StIO ())
changeEntityParameterParser = (flip (>>=) <$> (uncurry changeEntityParameter <$> entityParameterAccessorParser)) <*> parameterFunctionParser

-- |Match a conditional instruction.
conditionalParser :: Parser (StIO ())
conditionalParser = 
    (
        baseCodeLineParser "if " (conditionallyPerformAction <$> booleanExpressionParser <*> (stringWithSpaces "then" *> codeParser) <*> ((stringWithSpaces "else" *> codeParser) <|> pure noAction)) "conditional"
    )
    <?> "Conditional definition"

-- |Match a @take@ instruction.
takeItemParser :: ItemName -> Parser (StIO ())
takeItemParser item = baseCodeLineParser "take" (pure $ takeItem item) "take"

-- |Match a @drop@ instruction.
dropItemParser :: ItemName -> Parser (StIO ())
dropItemParser item = baseCodeLineParser "drop" (pure $ dropItem item) "drop"

-- |Match a @discard@ instruction.
discardItemParser :: ItemName -> Parser (StIO ())
discardItemParser item = baseCodeLineParser "discard" (pure $ discardItem item) "discard"

-- |Match a @give <item name>@ instruction.
giveItemParser :: Parser (StIO ())
giveItemParser = baseCodeLineParser "give " (giveItem . unpack <$> takeTill isSpace) "give"

-- |Match a @put <item name>@ instruction.
putItemParser :: Parser (StIO ())
putItemParser = baseCodeLineParser "put " (giveItem . unpack <$> takeTill isSpace) "put"

-- |Match a single code line (or a conditional instruction)
codeLineParser :: Parser (StIO ())
codeLineParser = 
    (skipSpaces *> (
        gotoParser 
        <|> printEmptyLineParser
        <|> printParser
        <|> printLineParser
        <|> printValueParser
        <|> changePlayerParameterParser
        <|> changeEntityParameterParser
        <|> giveItemParser
        <|> putItemParser
        <|> conditionalParser
    )) 
    <?> "Command definition"

-- |Match the code block where single line is matched by 'Parser (StIO ())'.
baseCodeParser :: Parser (StIO ()) -> Parser (StIO ())
baseCodeParser parser = openCurlyBrace *> (Prelude.foldr <$> pure (>>) <*> pure noAction <*> many' parser) <* skipSpaces <* closeCurlyBrace

-- |Match a code block with common instructions.
codeParser :: Parser (StIO ())
codeParser = baseCodeParser codeLineParser

-- |Match single code line with instructions specific to item.
itemCodeLineParser :: ItemName -> Parser (StIO ())
itemCodeLineParser item = 
    (skipSpaces *> (
        codeLineParser
        <|> takeItemParser item
        <|> dropItemParser item
        <|> discardItemParser item
    ))
    <?> "Item command definition"

-- |Match a code block with instructions specific to item.
itemCodeParser :: ItemName -> Parser (StIO ())
itemCodeParser item = baseCodeParser $ itemCodeLineParser item