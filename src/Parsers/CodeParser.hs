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
        baseCodeLineParser "if " (conditionallyEvaluateAction <$> booleanExpressionParser <*> (stringWithSpaces "then" *> parser) <*> ((stringWithSpaces "else" *> (parser <|> conditionalParserF parser)) <|> (pure $ const noAction))) "conditional"
    )
    <?> "Conditional definition"

-- |Match a conditional instruction.
conditionalParser :: Parser (Action ()) -> Parser (Action ())
conditionalParser parser = conditionalParserF (const <$> parser) <*> pure ()

-- |Match a @take@ instruction.
takeSelfParser :: Parser (ItemName -> Action ())
takeSelfParser = baseCodeLineParser "take" (pure takeItem) "take"

-- |Match a @drop@ instruction.
dropSelfParser :: Parser (ItemName -> Action ())
dropSelfParser = baseCodeLineParser "drop" (pure dropItem) "drop"

-- |Match a @take <item name>@ instruction.
takeItemParser :: Parser (Action ())
takeItemParser = baseCodeLineParser "take " (takeItem . unpack <$> takeTill isSpace) "take"

-- |Match a @drop <item name>@ instruction.
dropItemParser :: Parser (Action ())
dropItemParser = baseCodeLineParser "drop " (dropItem . unpack <$> takeTill isSpace) "drop"

-- |Match a @dropall@ instruction.
dropAllParser :: Parser (Action ())
dropAllParser = baseCodeLineParser "dropall" (pure dropAllItemsInCurrentRoom) "dropall"

-- |Match a @discard@ instruction.
discardItemParser :: Parser (ItemName -> Action ())
discardItemParser = baseCodeLineParser "discard" (pure discardItem) "discard"

-- |Match a @give <item name>@ instruction.
giveItemParser :: Parser (Action ())
giveItemParser = baseCodeLineParser "give " (giveItem . unpack <$> takeTill isSpace) "give"

-- |Match a @put <item name>@ instruction.
putItemParser :: Parser (Action ())
putItemParser = baseCodeLineParser "put " (putItem . unpack <$> takeTill isSpace) "put"

-- |Match a @remove <item name>@ instruction.
removeItemParser :: Parser (Action ())
removeItemParser = baseCodeLineParser "remove " (discardItem . unpack <$> takeTill isSpace) "remove"

removeItemFromRoomParser :: Parser (Action ())
removeItemFromRoomParser = baseCodeLineParser "remove " (removeItemFromRoom <$> (unpack <$> takeTill isSpace) <*> (string " from " *> (unpack <$> takeTill isSpace))) "remove from"

putItemInRoomParser :: Parser (Action ())
putItemInRoomParser = baseCodeLineParser "put " (putItemInRoom <$> (unpack <$> takeTill isSpace) <*> (string " in " *> (unpack <$> takeTill isSpace))) "put in"

moveItemToRoomParser :: Parser (Action ())
moveItemToRoomParser = baseCodeLineParser "move " (moveItemToRoom <$> (unpack <$> takeTill isSpace) <*> (string " to " *> (unpack <$> takeTill isSpace))) "move to"

-- |Match a @call <command>@ instruction for a room.
callForRoomParser :: Parser (Action ())
callForRoomParser = baseCodeLineParser "call " (callCommandForCurrentRoom . unpack <$> takeTill isSpace) "call"

-- |Match a @quit@ or @exit@ instruction.
quitParser :: Parser (Action ())
quitParser = baseCodeLineParser "quit" (pure $ getFinalMessage >>= terminate) "quit"

-- |Match a comment.
commentParser :: Parser (Action ())
commentParser = baseCodeLineParser "#" (takeTill isNewline *> pure noAction) "comment"

-- |Match a @save@ instruction.
saveParser :: Parser (Action ())
saveParser = baseCodeLineParser "save" (pure saveGame) "save"

-- |Match a @restore@ instruction.
restoreParser :: Parser (Action ())
restoreParser = baseCodeLineParser "restore" (pure restoreGame) "restore"

-- |Match a @showInventory@ instruction.
showInventoryParser :: Parser (Action ())
showInventoryParser = baseCodeLineParser "showInventory" (pure showInventory) "show inventory"

-- |Match a @look@ instruction.
lookParser :: Parser (Action ())
lookParser = baseCodeLineParser "look" (pure $ void showRoom) "look"

-- |Match a @globalOnEntry@ instruction. 
globalOnEntryCallParser :: Parser (Action ())
globalOnEntryCallParser = baseCodeLineParser "globalOnEntry" (pure callGlobalOnEntry) "global onEntry"

-- |Match a single code line common for items and the rest.
commonCodeLineParser :: Parser (Action ())
commonCodeLineParser = 
    (skipSpaces *> (
        commentParser
        <|> lookParser
        <|> gotoParser 
        <|> printEmptyLineParser
        <|> printParser
        <|> printLineParser
        <|> printValueParser
        <|> dropAllParser
        <|> changePlayerParameterParser
        <|> changeEntityParameterParser
        <|> removeItemFromRoomParser
        <|> putItemInRoomParser
        <|> moveItemToRoomParser
        <|> giveItemParser
        <|> putItemParser
        <|> takeItemParser
        <|> dropItemParser
        <|> removeItemParser
        <|> callForRoomParser
        <|> quitParser
        <|> saveParser
        <|> restoreParser
        <|> showInventoryParser
        <|> globalOnEntryCallParser
        <|> globalRoomParser
        <|> globalExplicitItemParser
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

-- |Match a code block with common instructions.
codeParser :: Parser (Action ())
codeParser = openCurlyBrace *> (Prelude.foldr <$> pure (>>) <*> pure noAction <*> many' codeLineParser) <* skipSpaces <* closeCurlyBrace

hasConditionalParser :: Parser (ItemName -> Action ()) ->  Parser (ItemName -> Action ())
hasConditionalParser parser = stringWithSpaces "if has then" *> (((\f g h -> (\n -> f n g h n)) $ (conditionallyEvaluateAction . checkIfItemIsInInventory)) <$> parser <*> ((stringWithSpaces "else" *> parser) <|> pure (const noAction))) <?> "'has' conditional"

presentConditionalParser :: Parser (ItemName -> Action ()) -> Parser (ItemName -> Action ())
presentConditionalParser parser = stringWithSpaces "if present then" *> (((\f g h -> (\n -> f n g h n)) $ (conditionallyEvaluateAction . checkIfInteractablePresent)) <$> parser <*> ((stringWithSpaces "else" *> parser) <|> pure (const noAction))) <?> "'present' conditional"

-- |Match single code line with instructions specific to item.
itemCodeLineParser :: Parser (ItemName -> Action ())
itemCodeLineParser = 
    (skipSpaces *> (
        (pure const <*> commonCodeLineParser)
        <|> takeSelfParser
        <|> dropSelfParser
        <|> discardItemParser
        <|> globalItemParser
        <|> (conditionalParserF itemCodeParser)
        <|> (hasConditionalParser itemCodeParser) 
        <|> (presentConditionalParser itemCodeParser))
    )
    <?> "Item command definition"

-- |Match a code block with instructions specific to item.
itemCodeParser :: Parser (ItemName -> Action ())
itemCodeParser = openCurlyBrace *> (Prelude.foldr <$> pure (\f g -> (\n -> f n >> g n)) <*> pure (const noAction) <*> many' itemCodeLineParser) <* skipSpaces <* closeCurlyBrace

-- |Match a @global room.<command>@ instruction.
globalRoomParser :: Parser (Action ())
globalRoomParser = baseCodeLineParser "global room." (callGlobalRoomCommand . unpack <$> takeTill isSpace) "global room command call"

-- |Match a @global item.<command>@ instruction.
globalItemParser :: Parser (ItemName -> Action ())
globalItemParser = baseCodeLineParser "global item." (callGlobalItemCommand . unpack <$> takeTill isSpace) "global item command call"

-- |Match a @global item.<command> <argument>@ instruction.
globalExplicitItemParser :: Parser (Action ())
globalExplicitItemParser = globalItemParser <*> (char ' ' *> (unpack <$> takeTill isSpace))