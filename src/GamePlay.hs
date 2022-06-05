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

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

saveGameCommand :: StIO ()
saveGameCommand = do
  filename <- lift $ putStr "File name: " >> hFlush stdout >> getLine
  game <- get
  lift $ exportGameStateToFile filename game

restoreGameCommand :: StIO ()
restoreGameCommand = do
  maybeGameState <- lift $ putStr "File name: " >> hFlush stdout >> getLine >>= importGameStateFromFile
  game <- get
  put $ maybe game (flip setGameState game) maybeGameState

constantCommands :: [(String, Action Bool)]
constantCommands = [
  ("save", Perform saveGameCommand),
  ("restore", Perform restoreGameCommand),
  ("inventory", showInventory),
  ("quit", showFinalMessage >> Terminate)
 ]

combinedCommands :: [Command] -> M.Map String (Action Bool)
combinedCommands cmds = M.fromList $ (map (\(_,n,r) -> (n, r)) cmds) ++ constantCommands

unknownCommand :: Action ()
unknownCommand = Perform $ lift $ putStrLn "Unknown command"

getInitialMessage :: StIO String
getInitialMessage = gets (\g -> initialMessage g)

getCurrentRoom :: Action Room
getCurrentRoom = Perform $ gets (\g -> rooms g M.! currentRoomName g)

getCommandsForRoom :: Action [Command]
getCommandsForRoom = Perform $ gets (\g -> filter fst3 $
  (globalRoomCommands g ++ (roomCommands $ rooms g M.! currentRoomName g))
  ++ (foldl' (++) [] 
      $ map 
        (\itName -> map 
                    (\(b,n,stio) -> (b,n ++ " " ++ itName,stio)) 
                    $ (globalItemCommands g itName) ++ (getCommands $ globalNameMap g M.! itName)) 
        $ (interactables $ rooms g M.! currentRoomName g) ++ (playerInventory $ player g)))

processCommand :: String -> Action ()
processCommand cmd = getCommandsForRoom >>= (\cmds -> M.findWithDefault unknownCommand cmd $ combinedCommands cmds)

readCommand :: Action ()
readCommand = Perform (lift (putChar '>' >> hFlush stdout >> getLine)) >>= processCommand

mainStart :: StIO ()
mainStart = getInitialMessage >>= (lift . putStrLn) >> mainLoop showAndExecuteOnEntryCurrentRoom

mainLoop :: Action () -> StIO ()
mainLoop Terminate = return ()
mainLoop (Perform _) = mainLoop readCommand