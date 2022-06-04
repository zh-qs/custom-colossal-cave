{-# LANGUAGE OverloadedStrings #-}

module GameParser where

import DataStructures
import qualified Data.Map.Strict as M
import Data.Attoparsec.Text
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Text
import Data.Text.IO
import Data.Char
import Commands
import Control.Applicative
import Data.Maybe
import Parsers.CodeParser
import Parsers.PlayerParser
import Parsers.Utilities
import Parsers.RoomParser
import Parsers.ItemParser
import Parsers.GlobalCommandsParser
import Parsers.SwitchParser

startRoomParser :: StParser Name
startRoomParser = lift ((string "start: " *> (unpack <$> takeWhile1 isAlphaNum) <* newLines) <?> "Start room name")

initialMessageParser :: StParser String
initialMessageParser = lift $ multilineContentParser "initialMessage" 0 "Initial message definition"

finalMessageParser :: StParser Desc
finalMessageParser = lift $ switchParser "finalMessage" 0 "Final message definition"

gameParserSt :: StParser (M.Map Name Interactable -> Game)
gameParserSt = 
    (Game
        <$> playerParser
        <*> initialMessageParser
        <*> (finalMessageParser <* itemListParser "items" 1 "Initial item list")
        <*> (globalHeaderParser *> globalRoomCommandsParser)
        <*> globalItemCommandsParser
        <*> roomMapParser
        <*> startRoomParser)
    <??> "Game definition"
    where
        roomMapParser = M.fromList <$> roomListParser -- PRZEROBIĆ TAK, ŻEBY DZIAŁAŁ PARITAMI!!!!!

gameParser :: Parser Game
gameParser = (\(a,s) -> a s) <$> runStateT gameParserSt M.empty 

checkGame :: Game -> Either String Game
checkGame g = if M.member (currentRoomName g) (rooms g) 
    then Right g
    else Left "Unknown start room name"

parseGame :: Text -> Either String Game 
parseGame gaml = (eitherResult' $ feed (parse (gameParser <* endOfInput) gaml) "") >>= checkGame -- ZMIENIĆ BO NIE DZIAŁA!!!

parseGameFromFile :: FilePath -> IO (Either String Game)
parseGameFromFile path = parseGame <$> Data.Text.IO.readFile path

testGameParser :: Result Game
testGameParser = feed 
    (parse (gameParser <* endOfInput)
        "player:\n\
        \  parameters:\n\
        \    - life:\n\
        \        value: 10\n\
        \  inventory:\n\
        \  leftHand:\n\
        \    empty\n\
        \  rightHand:\n\
        \    empty\n\
        \initialMessage:\n\
        \  Witaj!\n\
        \rooms:\n\
        \  - jeden:\n\
        \      description:\n\ 
        \        Pierwszy pokoj\n\
        \      items:\n\
        \        - axe:\n\
        \            commands:\n\
        \      commands:\n\
        \        - nop:\n\
        \        {\n\
        \          print Do nothing\n\
        \          goto jeden\n\
        \        }\n\
        \\n\
        \start: jeden\n"
    ) ""
