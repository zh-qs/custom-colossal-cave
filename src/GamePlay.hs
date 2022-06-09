-- |Contains functions responsible for handling user input and executing proper actions.
module GamePlay where

import DataStructures
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import System.Environment
import System.IO.Error
import System.IO
import Data.List
import Commands
import qualified Data.Map.Strict as M

-- |Return a first element of a triple.
fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

-- |Converts command list to a map with 'Name' as a key and 'Action' as a value.
toCommandMap :: [Command] -> M.Map Name (Action ())
toCommandMap cmds = M.fromList $ map (\(_,n,r) -> (n, r)) cmds

-- |Retrieves an 'initialMessage' from a state.
getInitialMessage :: StIO String
getInitialMessage = gets (\g -> initialMessage g)

-- |Gets all possible commands for a current room. Concatenates global room commands, current room commands
-- and for every item add its commands and global item commands, which name is appended with item name.
getCommandsForRoom :: Action ([Command],Action ())
getCommandsForRoom = perform $ gets (\g -> (filter fst3 $
  (globalRoomCommands g ++ (roomCommands $ rooms g M.! currentRoomName g))
  ++ (foldl' (++) [] 
      $ map 
        (\itName -> map 
                    (\(b,n,stio) -> (b,n ++ " " ++ itName,stio)) 
                    $ (globalItemCommands g itName) ++ (getCommands $ globalNameMap g M.! itName)) 
        $ (interactables $ rooms g M.! currentRoomName g) ++ (playerInventory $ player g)),
  unknownCommandMessage g))

-- |Returns a performed action, where 'String' is an entered command.
processCommand :: String -> Action ()
processCommand cmd = increaseCommandCount >> getCommandsForRoom >>= (\(cmds,unknown) -> M.findWithDefault unknown cmd $ toCommandMap cmds)

-- |Reads command from 'stdin' and passes it to 'processCommand'.
readCommand :: Action ()
readCommand = perform (lift (putChar '>' >> hFlush stdout >> getLine)) >>= processCommand

-- |Starts a game: prints an initial message and starts a main game loop.
mainStart :: StIO ()
mainStart = getInitialMessage >>= (lift . putStrLn) >> mainLoop showAndExecuteOnEntryCurrentRoom

-- |Loop function for a game: wait for an input and then execute an 'Action'. If 'Action' is the 'terminate' function result, break the loop and exit.
mainLoop :: Action () -> StIO ()
mainLoop a = fromAction a >>= either (lift . putStrLn) (const $ mainLoop readCommand)