module GamePlay where

import DataStructures
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import System.Environment
import System.IO.Error
import System.IO
import GameIO
import Data.List
import Commands
import qualified Data.Map.Strict as M

saveGameCommand :: StIO ()
saveGameCommand = return ()

restoreGameCommand :: StIO ()
restoreGameCommand = return ()

constantCommands :: [(String, StIO Bool)]
constantCommands = [
  ("save", saveGameCommand >> return True),
  ("restore", restoreGameCommand >> return True),
  ("inventory", showInventory >> return True),
  ("quit", return False)
 ]

combinedCommands :: [Command] -> M.Map String (StIO Bool)
combinedCommands cmds = M.fromList $ (map (\(n,r) -> (n, r >> return True)) cmds)++constantCommands

unknownCommand :: StIO Bool
unknownCommand = (lift $ putStrLn "Unknown command") >> return True

getInitialMessage :: StIO String
getInitialMessage = gets (\g -> initialMessage g)

getCurrentRoom :: StIO Room
getCurrentRoom = gets (\g -> rooms g M.! currentRoomName g)

getCommandsForRoom :: StIO [Command]
getCommandsForRoom = gets (\g -> 
  (roomCommands $ rooms g M.! currentRoomName g)
  ++ (foldl' (++) [] $ map (\it -> map (\(n,stio) -> (n ++ " " ++ itemName it,stio)) $ itemCommands it) $ items $ rooms g M.! currentRoomName g)
  ++ (foldl' (++) [] $ map (\it -> map (\(n,stio) -> (n ++ " " ++ itemName it,stio)) $ itemCommands it) $ playerInventory $ player g))

processCommand :: String -> StIO Bool
processCommand cmd = getCommandsForRoom >>= (\cmds -> M.findWithDefault unknownCommand cmd $ combinedCommands cmds)

readCommand :: StIO Bool
readCommand = lift (putChar '>' >> hFlush stdout >> getLine) >>= processCommand

mainStart :: StIO ()
mainStart = getInitialMessage >>= (lift . putStrLn) >> getCurrentRoom >>= (lift . print) >> mainLoop

mainLoop :: StIO ()
mainLoop = do
  result <- readCommand
  if result
    then mainLoop
    else return ()