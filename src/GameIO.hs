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

data InteractableState = ItemState | EntityState { entityStateParameters :: M.Map Name Int } deriving (Show, Read)

data RoomState = RoomState {
    interactableStates :: [Name]
    } deriving (Show, Read)

data GameState = GameState { 
    playerState :: Player, 
    commandCountState :: Int,
    roomStates :: (M.Map Name RoomState), 
    currentRoomStateName :: Name,
    globalNameStateMap :: M.Map Name InteractableState 
    } deriving (Show, Read)

toInteractableState :: Interactable -> InteractableState
toInteractableState (Item {}) = ItemState
toInteractableState (Entity _ params _) = EntityState params

toRoomState :: Room -> RoomState
toRoomState = RoomState . interactables

toGameState :: Game -> GameState
toGameState (Game m p im fm goe gr gi ccnt rs n imap) = GameState p ccnt (toRoomState <$> rs) n (toInteractableState <$> imap)

setRoomState :: Name -> RoomState -> Room -> Room
setRoomState _ (RoomState ists) (Room d e _ rcmds) = Room d e ists rcmds

setRoomStates :: M.Map Name RoomState -> M.Map Name Room -> M.Map Name Room
setRoomStates = M.merge M.dropMissing M.dropMissing (M.zipWithMatched setRoomState) 

setInteractableState :: Name -> InteractableState -> Interactable -> Interactable
setInteractableState _ ItemState i@(Item {}) = i
setInteractableState _ (EntityState params) (Entity desc _ cmds) = Entity desc params cmds
setInteractableState _ _ _ = Invalid -- unmatched types

setInteractableStates :: M.Map Name InteractableState -> M.Map Name Interactable -> M.Map Name Interactable
setInteractableStates = M.merge M.dropMissing M.dropMissing (M.zipWithMatched setInteractableState)

setGameState :: GameState -> Game -> Game
setGameState (GameState pst ccnt rsts n istmap) (Game m _ im fm goe gr gi _ rs _ imap) = Game m pst im fm goe gr gi ccnt (setRoomStates rsts rs) n (setInteractableStates istmap imap)

exportGameStateToFile :: FilePath -> Game -> IO ()
exportGameStateToFile fp g = writeFile fp $ show $ toGameState g

importGameStateFromFile :: FilePath -> IO (Maybe GameState)
importGameStateFromFile fp = readMaybe <$> readFile fp