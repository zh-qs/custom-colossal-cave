{-# LANGUAGE OverloadedStrings #-}

-- |Provides parsers matching a whole file content (and returning a 'Game' object) 
-- and also functions reading file content and parsing a game.
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

-- |Match a @startRoom@ property.
startRoomParser :: StParser Name
startRoomParser = lift ((string "start: " *> (unpack <$> takeWhile1 isAlphaNum) <* newLines) <?> "Start room name")

-- |Parse an initial message, printed once at a startup.
initialMessageParser :: StParser String
initialMessageParser = lift $ multilineContentParser "initialMessage" 0 "Initial message definition"

-- |Parse a final message (which uses 'switchParser')
finalMessageParser :: StParser Desc
finalMessageParser = lift $ switchParser "finalMessage" 0 "Final message definition"

-- |Parse an 'unknownCommandMessage's list.
unknownCommandMessageParser :: StParser (Action ())
unknownCommandMessageParser = lift $ 
    ((>>=printMessageLine) <$> (getRandomListElement <$> listParser "unknownCommandMessages" ((unpack <$> takeTill isNewline) <* newLines) 1 "Unknown command messages definition")) 
    <|> (pure $ printMessageLine "Unknown command")

-- |A stateful parser, which returns function from state being hold (a map from item name to item object) to 'Game' structure.
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

-- |Parse a whole file content; run a stateful 'gameParserSt' and apply 'StParser''s state to a returned function.
gameParser :: Parser Game
gameParser = (\(f,(s,_)) -> f s) <$> runStateT gameParserSt (M.empty, noAction) 

-- |Checks if start room name is valid (if it points to an existent room).
checkGame :: Game -> Either String Game
checkGame g = if M.member (currentRoomName g) (rooms g) 
    then Right g
    else Left "Unknown start room name"

-- |Parse a 'Text' as GAML content. Returns 'Left' with an error message in case of unsuccesful parse.
parseGame :: Text -> Either String Game 
parseGame gaml = (eitherResult' $ feed (parse (gameParser <* endOfInput) gaml) "") >>= checkGame

-- |Parses the game from a file at given path.
parseGameFromFile :: FilePath -> IO (Either String Game)
parseGameFromFile path = parseGame <$> Data.Text.IO.readFile path
