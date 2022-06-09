-- |This module contains types and functions used to save and to read the game state from a file.
-- The @state@ types contains only values from corresponding types defined in module 'DataStructures',
-- that can be changed during a game.
module GameIO where

import DataStructures
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import System.Environment
import System.IO.Error
import System.IO
import Data.List
import Text.Read
import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict as M

-- |State of an item, contains its parameter map.
data InteractableState = ItemState { itemStateParameters :: M.Map Name Int } deriving (Show, Read)

-- |State of a room, contains item list present inside it.
data RoomState = RoomState {
    interactableStates :: [ItemName]
    } deriving (Show, Read)

-- |State of an entire game, contains player structure, command counter, states of rooms, current room name and states of items.
data GameState = GameState { 
    playerState :: Player, 
    commandCountState :: Int,
    roomStates :: (M.Map Name RoomState), 
    currentRoomStateName :: Name,
    globalNameStateMap :: M.Map Name InteractableState 
    } deriving (Show, Read)

-- |Converts an 'Interactable' to its state.
toInteractableState :: Interactable -> InteractableState
toInteractableState = ItemState . itemParameters

-- |Converts a 'Room' to its state.
toRoomState :: Room -> RoomState
toRoomState = RoomState . interactables

-- |Converts a 'Game' to its state.
toGameState :: Game -> GameState
toGameState (Game m p im fm goe gr gi ccnt rs n imap) = GameState p ccnt (toRoomState <$> rs) n (toInteractableState <$> imap)

-- |Applies a 'RoomState' to a given 'Room'. The 'Name' parameter is dummy and its only use is to match 'zipWithMatched' function definition.
setRoomState :: Name -> RoomState -> Room -> Room
setRoomState _ (RoomState ists) (Room d e _ rcmds) = Room d e ists rcmds

-- |Applies a 'Map Name RoomState' to a 'Map Name Room' of rooms.
setRoomStates :: M.Map Name RoomState -> M.Map Name Room -> M.Map Name Room
setRoomStates = M.merge M.dropMissing M.dropMissing (M.zipWithMatched setRoomState) 

-- |Applies a 'InteractableState' to a given 'Interactable'. The 'Name' parameter is dummy and its only use is to match 'zipWithMatched' function definition.
setInteractableState :: Name -> InteractableState -> Interactable -> Interactable
setInteractableState _ (ItemState params) (Item ln d _ cmds) = Item ln d params cmds

-- |Applies a 'Map Name InteractableState' to a 'Map Name Interactable' of items.
setInteractableStates :: M.Map Name InteractableState -> M.Map Name Interactable -> M.Map Name Interactable
setInteractableStates = M.merge M.dropMissing M.dropMissing (M.zipWithMatched setInteractableState)

-- |Applies a 'GameState' to a given 'Game'.
setGameState :: GameState -> Game -> Game
setGameState (GameState pst ccnt rsts n istmap) (Game m _ im fm goe gr gi _ rs _ imap) = Game m pst im fm goe gr gi ccnt (setRoomStates rsts rs) n (setInteractableStates istmap imap)

-- |Saves a game state from 'Game' to a file, which name is given in 'FilePath'. 
exportGameStateToFile :: FilePath -> Game -> IO ()
exportGameStateToFile fp g = writeFile fp $ show $ toGameState g

-- |Reads a game state from a file, which name is given in 'FilePath'. 
importGameStateFromFile :: FilePath -> IO (Maybe GameState)
importGameStateFromFile fp = readMaybe <$> readFile fp