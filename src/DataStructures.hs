module DataStructures where

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Attoparsec.Text
import Control.Applicative
import Control.Monad.IO.Class
import Data.List

type Name = String
type ParamName = String
type Id = Int
type ItemName = Name
type Inventory = [ItemName]
type Hand = Maybe ItemName
type Visibility = Bool

type StIO a = StateT Game IO a
type Desc = Action String

-- |Stateful 'Parser' with a map of interactables already read and default room entry action as a state.
type StParser a = StateT (M.Map Name Interactable, Action ()) Parser a

-- |Name a stateful 'Parser'. The lifted equivalent of '<?>' from attoparsec.
(<??>) :: StParser a -> String -> StParser a
(<??>) stp msg = StateT $ \s -> runStateT stp s <?> msg

-- |Representation of a command entered in a console by user
type Command = (Visibility, Name, Action ())

data Interactable = Item { longName :: Name, getDescription :: Desc, itemParameters :: M.Map Name Int, getCommands :: [Command] }

instance Show Interactable where
    show item = "Item(commands:" ++ foldl' (\s (b,n,_) -> s ++ " " ++ if b then "*" else "" ++ n) "" (getCommands item) ++ ")"

data Room = Room { description :: Desc, onEntry :: Action (), interactables :: [Name], roomCommands :: [Command] }

instance Show Room where
    show room = "Room(items:" ++ (foldl' (\str item -> str ++ " " ++ item) "" $ interactables room) ++ ")"

-- |Structure that holds player parameters and items currently toting.
data Player = Player { playerParameters :: M.Map Name Int, playerInventory :: Inventory, leftHand :: Hand, rightHand :: Hand } deriving (Show, Read)

-- |Structure that holds all information and state of the game.
data Game = Game { 
    unknownCommandMessage :: Action (),
    player :: Player, 
    initialMessage :: String, 
    finalMessage :: Desc,
    globalOnEntry :: Action (), 
    globalRoomCommands :: [Command], 
    globalItemCommands :: (ItemName -> [Command]),
    commandCount :: Int,
    rooms :: M.Map Name Room, 
    currentRoomName :: Name, 
    globalNameMap :: M.Map Name Interactable 
    } 

-- |Encapsulates a 'StIO a' monad with an ability of immediate termination of a flow.
newtype Action a = Action { fromAction :: StIO (Either String a) }

instance Functor Action where
    fmap f (Action s) = Action $ fmap (fmap f) s

instance Applicative Action where
    pure = Action . lift . return . Right
    (Action f) <*> (Action s) = Action $ do
        ef <- f
        es <- s
        return $ ef <*> es

instance Monad Action where
    return = pure
    (Action s) >>= f = Action $ s >>= either (lift . return . Left) (fromAction . f)

instance Alternative Action where
    empty = Action $ lift $ return $ Left ""
    (Action p) <|> (Action q) = Action $ p >>= either (const q) (const p)

instance MonadIO Action where
    liftIO = perform . lift

-- |Lift 'StIO' to an 'Action'.
perform :: StIO a -> Action a
perform = Action . (>>= return . Right)

-- |Terminate an action flow with a custom message in 'String'.
terminate :: String -> Action a
terminate = Action . lift . return . Left


