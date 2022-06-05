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

type StParser a = StateT (M.Map Name Interactable) Parser a

-- |Name a stateful 'Parser'. The lifted equivalent of '<?>' from attoparsec.
(<??>) :: StParser a -> String -> StParser a
(<??>) stp msg = StateT $ \s -> runStateT stp s <?> msg

-- |Representation of a command entered in a console by user
type Command = (Visibility, Name, Action ())

data Interactable = Item { longName :: Name, getDescription :: Desc, getCommands :: [Command] }
    | Entity { getDescription :: Desc, entityParameters :: M.Map Name Int, getCommands :: [Command] }
    | Invalid

--data Item = Item { itemCommands :: [Command] }

instance Show Interactable where
    show item@(Item {}) = "Item(commands:" ++ foldl' (\s (b,n,_) -> s ++ " " ++ if b then "*" else "" ++ n) "" (getCommands item) ++ ")"
    show entity@(Entity {}) = "Entity(commands:" ++ foldl' (\s (b,n,_) -> s ++ " " ++ if b then "*" else "" ++ n) "" (getCommands entity) ++ ")"

--data Entity = Entity { entityParameters :: M.Map Name Int, entityCommands :: [Command] }

data Room = Room { description :: Desc, onEntry :: Action (), interactables :: [Name], roomCommands :: [Command] }

instance Show Room where
    show room = "Room(items:" ++ (foldl' (\str item -> str ++ " " ++ item) "" $ interactables room) ++ ")"

showRoom :: Room -> Action ()
showRoom r = description r >>= perform . lift . putStrLn . (++(foldl' (\str item -> str ++ "There is " ++ item ++ " nearby.\n") "" $ interactables r))

data Player = Player { playerParameters :: M.Map Name Int, playerInventory :: Inventory, leftHand :: Hand, rightHand :: Hand } deriving (Show, Read)

data Game = Game { 
    player :: Player, 
    initialMessage :: String, 
    finalMessage :: Desc,
    globalRoomCommands :: [Command], 
    globalItemCommands :: (ItemName -> [Command]), 
    rooms :: M.Map Name Room, 
    currentRoomName :: Name, 
    globalNameMap :: M.Map Name Interactable 
    } 

-- data Action a = Perform (StIO a) | Terminate

-- instance Functor Action where
--     fmap _ Terminate = Terminate
--     fmap f (Perform s) = Perform $ f s 

-- instance Applicative Action where
--     pure a = Perform $ lift $ return a
--     (Perform f) <*> (Perform s) = Perform $ f <*> s
--     _ <*> _ = Terminate

-- instance Monad Action where
--     return = pure
--     Terminate >>= _ = Terminate
--     (Perform s) >>= f = do
--         a <- s


-- instance Alternative Action where
--     empty = Terminate
--     p@(Perform {}) <|> _ = p
--     Terminate <|> p = p

newtype Action a = Action { fromAction :: StIO (Either String a) }

perform :: StIO a -> Action a
perform = Action . (>>= return . Right)

terminate :: String -> Action a
terminate = Action . lift . return . Left

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
