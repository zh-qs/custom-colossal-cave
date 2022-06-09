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

unknownCommandMessageParser :: StParser (Action ())
unknownCommandMessageParser = lift $ 
    ((>>=printMessageLine) <$> (getRandomListElement <$> listParser "unknownCommandMessages" ((unpack <$> takeTill isNewline) <* newLines) 1 "Unknown command messages definition")) 
    <|> (pure $ printMessageLine "Unknown command")

gameParserSt :: StParser (M.Map Name Interactable -> Game)
gameParserSt = 
    (Game
        <$> unknownCommandMessageParser
        <*> playerParser
        <*> initialMessageParser
        <*> (finalMessageParser <* itemListParser "items" 1 "Initial item list")
        <*> (globalHeaderParser *> globalOnEntryParser)
        <*> globalRoomCommandsParser
        <*> globalItemCommandsParser
        <*> (pure 0)
        <*> roomMapParser
        <*> startRoomParser)
    <??> "Game definition"
    where
        roomMapParser = M.fromList <$> roomListParser

gameParser :: Parser Game
gameParser = (\(f,(s,_)) -> f s) <$> runStateT gameParserSt (M.empty, noAction) 

checkGame :: Game -> Either String Game
checkGame g = if M.member (currentRoomName g) (rooms g) 
    then Right g
    else Left "Unknown start room name"

parseGame :: Text -> Either String Game 
parseGame gaml = (eitherResult' $ feed (parse (gameParser <* endOfInput) gaml) "") >>= checkGame

parseGameFromFile :: FilePath -> IO (Either String Game)
parseGameFromFile path = parseGame <$> Data.Text.IO.readFile path
