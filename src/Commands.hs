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

getInventoryCount :: Action Int
getInventoryCount = perform $ gets (length . playerInventory . player)

assertNotNothing :: String -> Maybe a -> Action a
assertNotNothing message = maybe (terminate message) pure  

assertFound :: (String -> String) -> Either String a -> Action a
assertFound f = either (terminate . f) pure

showAndExecuteOnEntryCurrentRoom :: Action ()
showAndExecuteOnEntryCurrentRoom = perform ((\g -> rooms g !?? currentRoomName g) <$> get)
    >>= assertFound (\k -> "ERROR: Room name not found: " ++ k)
    >>= (\r -> 
        description r >>= perform . lift . putStrLn
        >> foldl' (\act name -> act >> perform (gets (\g -> globalNameMap g M.! name)) >>= getDescription >>= perform . lift . putStr) noAction (interactables r)
        >> onEntry r)

noAction :: Action ()
noAction = perform $ lift $ return ()

showInventory :: Action ()
showInventory = perform $ gets (\g -> (playerInventory $ player g, globalNameMap g)) >>= (\(inv,imap) -> lift $ putStrLn $ foldl' (\s itName -> s ++ longName (imap M.! itName) ++ "\n") "" inv)

getFinalMessage :: Action String
getFinalMessage = perform (gets finalMessage) >>= id

callCommandForCurrentRoom :: Name -> Action ()
callCommandForCurrentRoom name = perform (gets (\g -> lookup name $ map (\(_,n,r) -> (n,r)) $ roomCommands $ rooms g M.! currentRoomName g)) 
    >>= assertNotNothing ("ERROR: Command name not found: " ++ name) >>= id

takeItem :: ItemName -> Action ()
takeItem item = perform $ modify' (\(Game (Player ps i lh rh) im fm gr gi rs n itMap) -> 
    Game 
        (Player ps (item:i) lh rh) 
        im fm 
        gr gi
        (M.adjust 
            (\(Room d e its cmds) -> Room d e (filter (/=item) its) cmds)
            n rs) 
        n 
        itMap)

giveItem :: ItemName -> Action ()
giveItem item = perform $ modify' (\(Game (Player ps i lh rh) im fm gr gi rs n itMap) -> 
    Game 
        (Player ps (item:i) lh rh) 
        im fm 
        gr gi
        rs 
        n 
        itMap)

putItem :: ItemName -> Action ()
putItem item = perform $ modify' (\(Game p im fm gr gi rs n itMap) -> 
    Game 
        p
        im fm 
        gr gi
        (M.adjust 
            (\(Room d e its cmds) -> Room d e (item:its) cmds)
            n rs) 
        n 
        itMap)

dropItem :: ItemName -> Action ()
dropItem item = perform $ modify' (\(Game (Player ps i lh rh) im fm gr gi rs n itMap) -> 
    Game 
        (Player ps (filter (/= item) i) lh rh) 
        im fm 
        gr gi
        (M.adjust 
            (\(Room d e its cmds) -> Room d e (item:its) cmds)
            n rs) 
        n 
        itMap)

removeItem :: ItemName -> Action ()
removeItem item = perform $ modify' (\(Game p im fm gr gi rs n itMap) -> 
    Game 
        p
        im fm 
        gr gi
        (M.adjust 
            (\(Room d e its cmds) -> Room d e (filter (/=item) its) cmds)
            n rs) 
        n 
        itMap)

discardItem :: ItemName -> Action ()
discardItem item = perform $ modify' (\(Game (Player ps i lh rh) im fm gr gi rs n itMap) -> 
    Game 
        (Player ps (filter (/= item) i) lh rh) 
        im fm 
        gr gi
        (M.adjust 
            (\(Room d e its cmds) -> Room d e (filter (/=item) its) cmds)
            n rs) 
        n 
        itMap)

printMessage :: String -> Action ()
printMessage s = perform $ lift $ putStr s >> hFlush stdout

printMessageLine :: String -> Action ()
printMessageLine = perform . lift . putStrLn

printInt :: Int -> Action ()
printInt x = perform $ lift $ (putStr $ show x) >> hFlush stdout

goToRoom :: Name -> Action ()
goToRoom newName = perform (modify' (\(Game p im fm gr gi rs n itmap) -> Game p im fm gr gi rs newName itmap)) >> showAndExecuteOnEntryCurrentRoom
--gets (\(Game p im fm rs n itmap) -> rs M.! newName) >>= showRoom >> 
changePlayerParameter :: Name -> (Int -> Int) -> Action ()
changePlayerParameter name f = perform $ modify' (\(Game (Player ps i lh rh) im fm gr gi rs n itmap) -> Game (Player (M.adjust f name ps) i lh rh) im fm gr gi rs n itmap)

getPlayerParameter :: Name -> Action Int
getPlayerParameter name = perform (gets (\(Game p im fm gr gi rs n itMap) -> playerParameters p M.!? name)) >>= assertNotNothing ("ERROR: Parameter not found: player." ++ name)

changeEntityParameter :: Name -> Name -> (Int -> Int) -> Action ()
changeEntityParameter entityName name f = perform $ modify' (\(Game p im fm gr gi rs n imap) -> Game p im fm gr gi rs n (M.adjust (\e -> Entity (getDescription e) (M.adjust f name $ entityParameters e) $ getCommands e) entityName imap))

getEntityParameter :: Name -> Name -> Action Int
getEntityParameter entityName name = perform (gets (\g -> globalNameMap g M.!? entityName)) >>= assertNotNothing ("ERROR: Entity not found: " ++ entityName) 
    >>= (\e -> pure $ entityParameters e M.!? name) >>= assertNotNothing ("ERROR: Parameter not found: entity." ++ entityName ++ "." ++ name)
--getEntityParameter entityName name = perform $ gets (\g -> (entityParameters $ globalNameMap g M.! entityName) M.! name)

conditionallyPerformAction :: Action Bool -> Action () -> Action () -> Action ()
conditionallyPerformAction cond trueaction falseaction = cond >>= (\c -> if c then trueaction else falseaction)

conditionallyEvaluateAction :: Action Bool -> (a -> Action ()) -> (a -> Action ()) -> a -> Action ()
conditionallyEvaluateAction cond ftrue ffalse a = conditionallyPerformAction cond (ftrue a) (ffalse a)

checkIfItemIsInInventory :: ItemName -> Action Bool
checkIfItemIsInInventory name = perform $ gets (\g -> name `elem` (playerInventory . player) g)

checkIfInteractablePresent :: Name -> Action Bool
checkIfInteractablePresent name = perform $ gets (\g -> name `elem` (playerInventory . player) g || name `elem` interactables (rooms g M.! currentRoomName g))

saveGame :: Action ()
saveGame = perform $ do
  filename <- lift getLine
  game <- get
  lift $ exportGameStateToFile filename game

restoreGame :: Action ()
restoreGame = perform $ do
  maybeGameState <- lift $ getLine >>= importGameStateFromFile
  game <- get
  put $ maybe game (flip setGameState game) maybeGameState
