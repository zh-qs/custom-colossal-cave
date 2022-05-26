module DataStructures where

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Attoparsec.Text
import Data.List

type Desc = String
type Name = String
type ParamName = String
type Id = Int
type ItemName = Name
type Inventory = [ItemName]
type Hand = Maybe ItemName

type StIO a = StateT Game IO a

type StParser a = StateT (M.Map Name Interactable) Parser a

-- |Name a stateful 'Parser'. The lifted equivalent of '<?>' from attoparsec.
(<??>) :: StParser a -> String -> StParser a
(<??>) stp msg = StateT $ \s -> runStateT stp s <?> msg

-- |Representation of a command entered in a console by user
type Command = (Name, StIO ())

data Interactable = Item { getCommands :: [Command] }
    | Entity { entityParameters :: M.Map Name Int, getCommands :: [Command] }

--data Item = Item { itemCommands :: [Command] }

instance Show Interactable where
    show item@(Item _) = "Item(commands:" ++ foldl' (\s (n,_) -> s ++ " " ++ n) "" (getCommands item) ++ ")"
    show entity@(Entity _ _) = "Entity(commands:" ++ foldl' (\s (n,_) -> s ++ " " ++ n) "" (getCommands entity) ++ ")"

--data Entity = Entity { entityParameters :: M.Map Name Int, entityCommands :: [Command] }

data Room = Room { description :: Desc, interactables :: [Name], roomCommands :: [Command] }

instance Show Room where
    show room = description room ++ (foldl' (\str item -> str ++ "There is " ++ item ++ " nearby.\n") "" $ interactables room)

data Player = Player { playerParameters :: M.Map Name Int, playerInventory :: Inventory, leftHand :: Hand, rightHand :: Hand } deriving (Show, Read)

data Game = Game { player :: Player, initialMessage :: String, rooms :: M.Map Name Room, currentRoomName :: Name, globalNameMap :: M.Map Name Interactable } deriving Show
