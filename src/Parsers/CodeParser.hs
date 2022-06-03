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

-- |Match a conditional function instruction.
conditionalParserF :: Parser (a -> StIO ()) -> Parser (a -> StIO ())
conditionalParserF parser = 
    (
        baseCodeLineParser "if " (conditionallyEvaluateAction <$> booleanExpressionParser <*> (stringWithSpaces "then" *> parser) <*> ((stringWithSpaces "else" *> parser) <|> (pure $ const noAction))) "conditional"
    )
    <?> "Conditional definition"

-- |Match a conditional instruction.
conditionalParser :: Parser (StIO ()) -> Parser (StIO ())
conditionalParser parser = conditionalParserF (const <$> parser) <*> pure ()

-- |Match a @take@ instruction.
takeItemParser :: Parser (ItemName -> StIO ())
takeItemParser = baseCodeLineParser "take" (pure takeItem) "take"

-- |Match a @drop@ instruction.
dropItemParser :: Parser (ItemName -> StIO ())
dropItemParser = baseCodeLineParser "drop" (pure dropItem) "drop"

-- |Match a @discard@ instruction.
discardItemParser :: Parser (ItemName -> StIO ())
discardItemParser = baseCodeLineParser "discard" (pure discardItem) "discard"

-- |Match a @give <item name>@ instruction.
giveItemParser :: Parser (StIO ())
giveItemParser = baseCodeLineParser "give " (giveItem . unpack <$> takeTill isSpace) "give"

-- |Match a @put <item name>@ instruction.
putItemParser :: Parser (StIO ())
putItemParser = baseCodeLineParser "put " (giveItem . unpack <$> takeTill isSpace) "put"

-- |Match a @call <command>@ instruction for a room.
callForRoomParser :: Parser (StIO ())
callForRoomParser = baseCodeLineParser "call " (callCommandForCurrentRoom . unpack <$> takeTill isSpace) "call"

-- |Match a single code line common for items and the rest.
commonCodeLineParser :: Parser (StIO ())
commonCodeLineParser = 
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
        <|> callForRoomParser
    )) 
    <?> "Command definition"

-- |Match a single code line (or a conditional instruction)
codeLineParser :: Parser (StIO ())
codeLineParser = 
    (skipSpaces *> (
        commonCodeLineParser
        <|> conditionalParser codeParser
    )) 
    <?> "Command definition"

-- |Match the code block where single line is matched by 'Parser (StIO ())'.
--baseCodeParser :: Parser (StIO ()) -> Parser (StIO ())
--baseCodeParser parser = openCurlyBrace *> (Prelude.foldr <$> pure (>>) <*> pure noAction <*> many' parser) <* skipSpaces <* closeCurlyBrace

-- |Match a code block with common instructions.
codeParser :: Parser (StIO ())
codeParser = openCurlyBrace *> (Prelude.foldr <$> pure (>>) <*> pure noAction <*> many' codeLineParser) <* skipSpaces <* closeCurlyBrace

-- |Match single code line with instructions specific to item.
itemCodeLineParser :: Parser (ItemName -> StIO ())
itemCodeLineParser = 
    (skipSpaces *> (
        (pure const <*> commonCodeLineParser)
        <|> takeItemParser
        <|> dropItemParser
        <|> discardItemParser
        <|>  (conditionalParserF itemCodeParser))
    )
    <?> "Item command definition"

-- |Match a code block with instructions specific to item.
itemCodeParser :: Parser (ItemName -> StIO ())
itemCodeParser = openCurlyBrace *> (Prelude.foldr <$> pure (\f g -> (\n -> f n >> g n)) <*> pure (const noAction) <*> many' itemCodeLineParser) <* skipSpaces <* closeCurlyBrace