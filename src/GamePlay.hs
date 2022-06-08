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

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

combinedCommands :: [Command] -> M.Map String (Action ())
combinedCommands cmds = M.fromList $ map (\(_,n,r) -> (n, r)) cmds

getInitialMessage :: StIO String
getInitialMessage = gets (\g -> initialMessage g)

getCurrentRoom :: Action Room
getCurrentRoom = perform $ gets (\g -> rooms g M.! currentRoomName g)

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

processCommand :: String -> Action ()
processCommand cmd = increaseCommandCount >> getCommandsForRoom >>= (\(cmds,unknown) -> M.findWithDefault unknown cmd $ combinedCommands cmds)

readCommand :: Action ()
readCommand = perform (lift (putChar '>' >> hFlush stdout >> getLine)) >>= processCommand

mainStart :: StIO ()
mainStart = getInitialMessage >>= (lift . putStrLn) >> mainLoop showAndExecuteOnEntryCurrentRoom

mainLoop :: Action () -> StIO ()
mainLoop a = fromAction a >>= either (lift . putStrLn) (const $ mainLoop readCommand)