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

type StIO a = StateT Game IO a

type StParser a = StateT (M.Map ItemName Item) Parser a

-- |Name a stateful 'Parser'. The lifted equivalent of '<?>' from attoparsec.
(<??>) :: StParser a -> String -> StParser a
(<??>) stp msg = StateT $ \s -> runStateT stp s <?> msg

-- |Representation of a command entered in a console by user
type Command = (Name, StIO ())

data Item = Item { itemCommands :: [Command] }

instance Show Item where
    show item = "Item(commands:" ++ foldl' (\s (n,_) -> s ++ " " ++ n) "" (itemCommands item) ++ ")"

data Entity = Entity Name [Command] -- ...

data Room = Room { description :: Desc, items :: [ItemName], roomCommands :: [Command] }

instance Show Room where
    show room = description room ++ (foldl' (\str item -> str ++ "There is " ++ item ++ " nearby.\n") "" $ items room)

type Hand = Maybe ItemName

data Player = Player { playerParameters :: M.Map Name Int, playerInventory :: Inventory, leftHand :: Hand, rightHand :: Hand } deriving Show

data Game = Game { player :: Player, initialMessage :: String, rooms :: M.Map Name Room, currentRoomName :: Name, globalItemMap :: M.Map ItemName Item } deriving Show
