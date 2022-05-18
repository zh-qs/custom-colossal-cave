module DataStructures where

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.List

type Desc = String
type Name = String
type ParamName = String
type Id = Int
type Inventory = [Item]

type StIO a = StateT Game IO a

type Command = (Name, StIO ()) --ChangeRoom Id | ChangeParameter ParamName (a -> a) | PrintText String | TakeItem Item | DropItem Item

data Item = Item { itemName :: Name, itemCommands :: [Command] }

data Entity = Entity Name [Command] -- ...

data Room = Room { description :: Desc, items :: [Item], roomCommands :: [Command] }

type Hand = Maybe Item

data Player = Player { playerParameters :: M.Map Name Int, playerInventory :: Inventory, leftHand :: Hand, rightHand :: Hand } deriving Show

data Game = Game { player :: Player, initialMessage :: String, rooms :: M.Map Name Room, currentRoomName :: Name } deriving Show

instance Show Room where
    show room = description room ++ (foldl' (\str item -> str ++ "There is " ++ itemName item ++ " nearby.\n") "" $ items room)

instance Show Item where
    show item = itemName item ++ " (commands:" ++ foldl' (\s (n,f) -> s ++ " " ++ n) "" (itemCommands item) ++ ")"