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
printEmptyLineParser :: Parser (Action ())
printEmptyLineParser = baseCodeLineParser "print." (pure $ printMessage "\n") "print empty line"

-- |Match a @print <text>@ instruction.
printParser :: Parser (Action ())
printParser = baseCodeLineParser "print " (printMessage . unpack <$> takeTill isNewline) "print"

-- |Match a @println <text>@ instruction.
printLineParser :: Parser (Action ())
printLineParser = baseCodeLineParser "println " (printMessageLine . unpack <$> takeTill isNewline) "println"

-- |Match a @printval <expression>@ instruction.
printValueParser :: Parser (Action ())
printValueParser = baseCodeLineParser "printval " ((>>= printInt) <$> expressionParser) "printval"

-- |Match a @goto <room>@ instruction.
gotoParser :: Parser (Action ())
gotoParser = baseCodeLineParser "goto " (goToRoom . unpack <$> takeTill isSpace) "goto"

-- |Match an assignment instruction.
assignmentParser :: Parser (Action (Int -> Int))
assignmentParser = charWithSpaces '=' *> ((\sti -> return (\num _ -> num) <*> sti) <$> expressionParser)

-- |Match a modification instruction.
modificationParser :: Parser (Action (Int -> Int))
modificationParser = 
    stringWithSpaces "+=" *> ((\sti -> return (+) <*> sti) <$> expressionParser) 
    <|> stringWithSpaces "-=" *> ((\sti -> return (flip (-)) <*> sti) <$> expressionParser)
    <|> stringWithSpaces "*=" *> ((\sti -> return (*) <*> sti) <$> expressionParser) 

-- |Match either an assignment or a modification instruction.
parameterFunctionParser :: Parser (Action (Int -> Int))
parameterFunctionParser = assignmentParser <|> modificationParser

-- |Match an instruction of type @player.<parameter> =/+=/-=/*= <expression>@.
changePlayerParameterParser :: Parser (Action ())
changePlayerParameterParser = (flip (>>=) <$> (changePlayerParameter <$> playerParameterAccessorParser)) <*> parameterFunctionParser

-- |Match an instruction of type @entity.<entity name>.<parameter> =/+=/-=/*= <expression>@.
changeEntityParameterParser :: Parser (Action ())
changeEntityParameterParser = (flip (>>=) <$> (uncurry changeEntityParameter <$> entityParameterAccessorParser)) <*> parameterFunctionParser

-- |Match a conditional function instruction.
conditionalParserF :: Parser (a -> Action ()) -> Parser (a -> Action ())
conditionalParserF parser = 
    (
        baseCodeLineParser "if " (conditionallyEvaluateAction <$> booleanExpressionParser <*> (stringWithSpaces "then" *> parser) <*> ((stringWithSpaces "else" *> parser) <|> (pure $ const noAction))) "conditional"
    )
    <?> "Conditional definition"

-- |Match a conditional instruction.
conditionalParser :: Parser (Action ()) -> Parser (Action ())
conditionalParser parser = conditionalParserF (const <$> parser) <*> pure ()

-- |Match a @take@ instruction.
takeItemParser :: Parser (ItemName -> Action ())
takeItemParser = baseCodeLineParser "take" (pure takeItem) "take"

-- |Match a @drop@ instruction.
dropItemParser :: Parser (ItemName -> Action ())
dropItemParser = baseCodeLineParser "drop" (pure dropItem) "drop"

-- |Match a @discard@ instruction.
discardItemParser :: Parser (ItemName -> Action ())
discardItemParser = baseCodeLineParser "discard" (pure discardItem) "discard"

-- |Match a @give <item name>@ instruction.
giveItemParser :: Parser (Action ())
giveItemParser = baseCodeLineParser "give " (giveItem . unpack <$> takeTill isSpace) "give"

-- |Match a @put <item name>@ instruction.
putItemParser :: Parser (Action ())
putItemParser = baseCodeLineParser "put " (giveItem . unpack <$> takeTill isSpace) "put"

-- |Match a @call <command>@ instruction for a room.
callForRoomParser :: Parser (Action ())
callForRoomParser = baseCodeLineParser "call " (callCommandForCurrentRoom . unpack <$> takeTill isSpace) "call"

-- |Match a @quit@ or @exit@ instruction.
quitParser :: Parser (Action ())
quitParser = baseCodeLineParser "quit" (pure $ getFinalMessage >>= terminate) "quit"

-- |Match a single code line common for items and the rest.
commonCodeLineParser :: Parser (Action ())
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
        <|> quitParser
    )) 
    <?> "Command definition"

-- |Match a single code line (or a conditional instruction)
codeLineParser :: Parser (Action ())
codeLineParser = 
    (skipSpaces *> (
        commonCodeLineParser
        <|> conditionalParser codeParser
    )) 
    <?> "Command definition"

-- |Match the code block where single line is matched by 'Parser (Action ())'.
--baseCodeParser :: Parser (Action ()) -> Parser (Action ())
--baseCodeParser parser = openCurlyBrace *> (Prelude.foldr <$> pure (>>) <*> pure noAction <*> many' parser) <* skipSpaces <* closeCurlyBrace

-- |Match a code block with common instructions.
codeParser :: Parser (Action ())
codeParser = openCurlyBrace *> (Prelude.foldr <$> pure (>>) <*> pure noAction <*> many' codeLineParser) <* skipSpaces <* closeCurlyBrace

-- |Match single code line with instructions specific to item.
itemCodeLineParser :: Parser (ItemName -> Action ())
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
itemCodeParser :: Parser (ItemName -> Action ())
itemCodeParser = openCurlyBrace *> (Prelude.foldr <$> pure (\f g -> (\n -> f n >> g n)) <*> pure (const noAction) <*> many' itemCodeLineParser) <* skipSpaces <* closeCurlyBrace