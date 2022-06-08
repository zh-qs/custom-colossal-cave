-- |This module contains implementations of instructions and expressions provided by language.
module Commands where

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import DataStructures
import Data.List
import System.IO
import Data.Maybe
import GameIO
import Data.Either

-- |Find the value at a key. Returns 'Left k' when the element cannot be found.
(!??) :: Ord k => M.Map k a -> k -> Either k a
m !?? k = maybe (Left k) Right $ m M.!? k 

-- |Get player's inventory count.
getInventoryCount :: Action Int
getInventoryCount = perform $ gets (length . playerInventory . player)

-- |If 'Maybe a' is 'Nothing', terminate execution with a 'String' message.
assertNotNothing :: String -> Maybe a -> Action a
assertNotNothing message = maybe (terminate message) pure  

-- |If 'Either String a' is 'Left', terminate execution with a message constructed with function and 'Left' content.
assertFound :: (String -> String) -> Either String a -> Action a
assertFound f = either (terminate . f) pure

-- |Show the description of a current room and descriptions of items inside it.
showRoom :: Action Room
showRoom = perform ((\g -> rooms g !?? currentRoomName g) <$> get)
    >>= assertFound (\k -> "ERROR: Room name not found: " ++ k)
    >>= (\r -> 
        description r >>= perform . lift . putStrLn
        >> foldl' (\act name -> act >> perform (gets (\g -> globalNameMap g M.! name)) >>= getDescription >>= perform . lift . putStr) noAction (interactables r)
        >> pure r)

-- |Show the description of a current room and descriptions of items inside it, then execute 'onEntry' action.
showAndExecuteOnEntryCurrentRoom :: Action ()
showAndExecuteOnEntryCurrentRoom = showRoom >>= onEntry

-- |Do nothing. Used for folding actions as neutral element.
noAction :: Action ()
noAction = perform $ lift $ return ()

-- |Show 'longName's of items in inventory.
showInventory :: Action ()
showInventory = perform $ gets (\g -> (playerInventory $ player g, globalNameMap g)) >>= (\(inv,imap) -> lift $ putStrLn $ foldl' (\s itName -> s ++ longName (imap M.! itName) ++ "\n") "" inv)

-- |Get final message of a game.
getFinalMessage :: Action String
getFinalMessage = perform (gets finalMessage) >>= id

-- |Call a global room command. If specified command does not exist, terminate execution with message in 'String'.
callGlobalRoomCommandWithMessage :: String -> Name -> Action ()
callGlobalRoomCommandWithMessage message name = perform (gets (\g -> lookup name $ map (\(_,n,r) -> (n,r)) $ globalRoomCommands g)) 
    >>= assertNotNothing message >>= id

-- |Call a command in current room. If specified command does not exist, try to call global command.
callCommandForCurrentRoom :: Name -> Action ()
callCommandForCurrentRoom name = perform (gets (\g -> lookup name $ map (\(_,n,r) -> (n,r)) $ roomCommands $ rooms g M.! currentRoomName g)) 
    >>= maybe (callGlobalRoomCommandWithMessage ("ERROR: Command name not found: " ++ name) name) id 

-- |Call a global room command. If specified command does not exist, terminate execution with an error.
callGlobalRoomCommand :: Name -> Action ()
callGlobalRoomCommand name = callGlobalRoomCommandWithMessage ("ERROR: Global command name not found: " ++ name) name

-- |Call global item command, passing 'ItemName' as the argument. If specified command does not exist, terminate execution with an error.
callGlobalItemCommand :: Name -> ItemName -> Action ()
callGlobalItemCommand name itemName = perform (gets (\g -> lookup name $ map (\(_,n,r) -> (n,r)) $ globalItemCommands g itemName)) 
    >>= assertNotNothing ("ERROR: Global command name not found: " ++ name ++ " " ++ itemName) >>= id

-- |Call global 'onEntry' command.
callGlobalOnEntry :: Action ()
callGlobalOnEntry = perform (gets globalOnEntry) >>= id

-- |Take an item in current room into inventory.
takeItem :: ItemName -> Action ()
takeItem item = perform $ modify' (\(Game (Player ps i lh rh) im fm goe gr gi ccnt rs n itMap) -> 
    Game 
        (Player ps (item:i) lh rh) 
        im fm 
        goe gr gi ccnt
        (M.adjust 
            (\(Room d e its cmds) -> Room d e (filter (/=item) its) cmds)
            n rs) 
        n 
        itMap)

-- |Give an item to inventory.
giveItem :: ItemName -> Action ()
giveItem item = perform $ modify' (\(Game (Player ps i lh rh) im fm goe gr gi ccnt rs n itMap) -> 
    Game 
        (Player ps (item:i) lh rh) 
        im fm 
        goe gr gi ccnt
        rs 
        n 
        itMap)

-- |Put an item in a specified room.
putItemInRoom :: ItemName -> Name -> Action ()
putItemInRoom item room = perform $ modify' (\(Game p im fm goe gr gi ccnt rs n itMap) -> 
    Game 
        p
        im fm 
        goe gr gi ccnt
        (M.adjust 
            (\(Room d e its cmds) -> Room d e (item:its) cmds)
            room rs) 
        n 
        itMap)

-- |Put an item in a current room.
putItem :: ItemName -> Action ()
putItem item = perform $ modify' (\(Game p im fm goe gr gi ccnt rs n itMap) -> 
    Game 
        p
        im fm 
        goe gr gi ccnt
        (M.adjust 
            (\(Room d e its cmds) -> Room d e (item:its) cmds)
            n rs) 
        n 
        itMap)

-- |Drop an item from an inventory.
dropItem :: ItemName -> Action ()
dropItem item = perform $ modify' (\(Game (Player ps i lh rh) im fm goe gr gi ccnt rs n itMap) -> 
    Game 
        (Player ps (filter (/= item) i) lh rh) 
        im fm 
        goe gr gi ccnt
        (M.adjust 
            (\(Room d e its cmds) -> Room d e (item:its) cmds)
            n rs) 
        n 
        itMap)

-- |Move an item from current room to a specified room.
moveItemToRoom :: ItemName -> Name -> Action ()
moveItemToRoom item room = perform $ gets (\g -> item `elem` interactables (rooms g M.! currentRoomName g))
    >>= (\b -> if b 
        then modify' (\(Game p im fm goe gr gi ccnt rs n itMap) -> 
            Game 
                p
                im fm 
                goe gr gi ccnt
                (M.adjust 
                    (\(Room d e its cmds) -> Room d e (item:its) cmds)
                    room
                    (M.adjust 
                    (\(Room d e its cmds) -> Room d e (filter (/=item) its) cmds)
                    n rs)) 
                n 
                itMap)
        else lift (return ()))

-- |Remove an item from specified room.
removeItemFromRoom :: ItemName -> Name -> Action ()
removeItemFromRoom item room = perform $ modify' (\(Game p im fm goe gr gi ccnt rs n itMap) -> 
    Game 
        p
        im fm 
        goe gr gi ccnt
        (M.adjust 
            (\(Room d e its cmds) -> Room d e (filter (/=item) its) cmds)
            room rs) 
        n 
        itMap)

-- |Remove an item from current room.
removeItem :: ItemName -> Action ()
removeItem item = perform $ modify' (\(Game p im fm goe gr gi ccnt rs n itMap) -> 
    Game 
        p
        im fm 
        goe gr gi ccnt
        (M.adjust 
            (\(Room d e its cmds) -> Room d e (filter (/=item) its) cmds)
            n rs) 
        n 
        itMap)

-- |Discard an item (remove it from inventory and from current room).
discardItem :: ItemName -> Action ()
discardItem item = perform $ modify' (\(Game (Player ps i lh rh) im fm goe gr gi ccnt rs n itMap) -> 
    Game 
        (Player ps (filter (/= item) i) lh rh) 
        im fm 
        goe gr gi ccnt
        (M.adjust 
            (\(Room d e its cmds) -> Room d e (filter (/=item) its) cmds)
            n rs) 
        n 
        itMap)

-- |Drop all items from inventory.
dropAllItemsInCurrentRoom :: Action ()
dropAllItemsInCurrentRoom = perform $ modify' (\(Game (Player ps i lh rh) im fm goe gr gi ccnt rs n itMap) -> 
    Game 
        (Player ps [] lh rh) 
        im fm 
        goe gr gi ccnt
        (M.adjust 
            (\(Room d e its cmds) -> Room d e (i++its) cmds)
            n rs) 
        n 
        itMap)

-- |Get a number of commands (also incorrect) already entered by a user
getCommandCount :: Action Int
getCommandCount = perform $ gets commandCount

-- |Increase command count by one.
increaseCommandCount :: Action ()
increaseCommandCount = perform $ modify' (\(Game p im fm goe gr gi ccnt rs n imap) -> Game p im fm goe gr gi (ccnt+1) rs n imap)

-- |Print a 'String' to a user, without new line at the end.
printMessage :: String -> Action ()
printMessage s = perform $ lift $ putStr s >> hFlush stdout

-- |Print a 'String' to a user, with a new line at the end.
printMessageLine :: String -> Action ()
printMessageLine = perform . lift . putStrLn

-- |Print a number to a user.
printInt :: Int -> Action ()
printInt x = perform $ lift $ (putStr $ show x) >> hFlush stdout

-- |Go to specified room (changes 'currentRoomName').
goToRoom :: Name -> Action ()
goToRoom newName = perform (modify' (\(Game p im fm goe gr gi ccnt rs n itmap) -> Game p im fm goe gr gi ccnt rs newName itmap)) >> showAndExecuteOnEntryCurrentRoom

-- |Change specified player parameter with a function.
changePlayerParameter :: Name -> (Int -> Int) -> Action ()
changePlayerParameter name f = perform $ modify' (\(Game (Player ps i lh rh) im fm goe gr gi ccnt rs n itmap) -> Game (Player (M.adjust f name ps) i lh rh) im fm goe gr gi ccnt rs n itmap)

-- |Get specified player parameter. If a parameter does not exist, terminate execution with an error.
getPlayerParameter :: Name -> Action Int
getPlayerParameter name = perform (gets (\(Game p im fm goe gr gi ccnt rs n itMap) -> playerParameters p M.!? name)) >>= assertNotNothing ("ERROR: Parameter not found: player." ++ name)

-- |Change specified entity parameter with a function.
changeEntityParameter :: Name -> Name -> (Int -> Int) -> Action ()
changeEntityParameter entityName name f = perform $ modify' (\(Game p im fm goe gr gi ccnt rs n imap) -> Game p im fm goe gr gi ccnt rs n (M.adjust (\e -> Entity (getDescription e) (M.adjust f name $ entityParameters e) $ getCommands e) entityName imap))

-- |Get specified entity parameter. If a parameter does not exist, terminate execution with an error.
getEntityParameter :: Name -> Name -> Action Int
getEntityParameter entityName name = perform (gets (\g -> globalNameMap g M.!? entityName)) >>= assertNotNothing ("ERROR: Entity not found: " ++ entityName) 
    >>= (\e -> pure $ entityParameters e M.!? name) >>= assertNotNothing ("ERROR: Parameter not found: entity." ++ entityName ++ "." ++ name)

-- |Perform either first or second action, regarding of the result of 'Action Bool'.
conditionallyPerformAction :: Action Bool -> Action () -> Action () -> Action ()
conditionallyPerformAction cond trueaction falseaction = cond >>= (\c -> if c then trueaction else falseaction)

-- |The same as 'conditionallyPerformAction', but for parametrised actions.
conditionallyEvaluateAction :: Action Bool -> (a -> Action ()) -> (a -> Action ()) -> a -> Action ()
conditionallyEvaluateAction cond ftrue ffalse a = conditionallyPerformAction cond (ftrue a) (ffalse a)

-- |Return true if item is in inventory.
checkIfItemIsInInventory :: ItemName -> Action Bool
checkIfItemIsInInventory name = perform $ gets (\g -> name `elem` (playerInventory . player) g)

-- |Return true if item is either in inventory or in current room.
checkIfInteractablePresent :: Name -> Action Bool
checkIfInteractablePresent name = perform $ gets (\g -> name `elem` (playerInventory . player) g || name `elem` interactables (rooms g M.! currentRoomName g))

-- |Return true if specified room is current room.
checkIfRoomIsCurrent :: Name -> Action Bool
checkIfRoomIsCurrent name = perform $ gets ((==name) . currentRoomName)

-- |Return true if specified item is in specified room.
checkIfInteractablePresentInRoom :: Name -> Name -> Action Bool
checkIfInteractablePresentInRoom name room = perform (gets (\g -> rooms g M.!? room)) 
    >>= assertNotNothing ("ERROR: Room name not found: " ++ room) 
    >>= (\r -> pure (name `elem` interactables r))

-- |Prompt for file name and save the game state to a file.
saveGame :: Action ()
saveGame = perform $ do
  filename <- lift getLine
  game <- get
  lift $ exportGameStateToFile filename game

-- |Prompt for file name and restore the same state from a file.
restoreGame :: Action ()
restoreGame = perform $ do
  maybeGameState <- lift $ getLine >>= importGameStateFromFile
  game <- get
  put $ maybe game (flip setGameState game) maybeGameState
