module Commands where

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import DataStructures
import Data.List
import System.IO
import Data.Maybe

showCurrentRoom :: StIO ()
showCurrentRoom = ((\g -> rooms g M.! currentRoomName g) <$> get)
    >>= (\r -> 
        description r >>= lift . putStrLn
        >> foldl' (\stio name -> stio >> gets (\g -> globalNameMap g M.! name) >>= getDescription >>= lift . putStrLn) (lift $ return ()) (interactables r))

noAction :: StIO ()
noAction = lift $ return ()

showInventory :: StIO ()
showInventory = gets (playerInventory . player) >>= (\inv -> lift $ putStrLn $ foldl' (\s itName -> s ++ itName ++ "\n") "Your inventory:\n" inv)

callCommandForCurrentRoom :: Name -> StIO ()
callCommandForCurrentRoom name = gets (\g -> fromJust $ lookup name $ map (\(_,n,r) -> (n,r)) $ roomCommands $ rooms g M.! currentRoomName g) >>= id

takeItem :: ItemName -> StIO ()
takeItem item = modify' (\(Game (Player ps i lh rh) im rs n itMap) -> 
    Game 
        (Player ps (item:i) lh rh) 
        im 
        (M.adjust 
            (\(Room d its cmds) -> Room d (filter (/=item) its) cmds)
            n rs) 
        n 
        itMap)

giveItem :: ItemName -> StIO ()
giveItem item = modify' (\(Game (Player ps i lh rh) im rs n itMap) -> 
    Game 
        (Player ps (item:i) lh rh) 
        im 
        rs 
        n 
        itMap)

putItem :: ItemName -> StIO ()
putItem item = modify' (\(Game p im rs n itMap) -> 
    Game 
        p
        im 
        (M.adjust 
            (\(Room d its cmds) -> Room d (item:its) cmds)
            n rs) 
        n 
        itMap)

dropItem :: ItemName -> StIO ()
dropItem item = modify' (\(Game (Player ps i lh rh) im rs n itMap) -> 
    Game 
        (Player ps (filter (/= item) i) lh rh) 
        im 
        (M.adjust 
            (\(Room d its cmds) -> Room d (item:its) cmds)
            n rs) 
        n 
        itMap)

discardItem :: ItemName -> StIO ()
discardItem item = modify' (\(Game (Player ps i lh rh) im rs n itMap) -> 
    Game 
        (Player ps (filter (/= item) i) lh rh) 
        im 
        (M.adjust 
            (\(Room d its cmds) -> Room d (filter (/=item) its) cmds)
            n rs) 
        n 
        itMap)

printMessage :: String -> StIO ()
printMessage s = lift $ putStr s >> hFlush stdout

printMessageLine :: String -> StIO ()
printMessageLine = lift . putStrLn

printInt :: Int -> StIO ()
printInt x = lift $ (putStr $ show x) >> hFlush stdout

goToRoom :: Name -> StIO ()
goToRoom newName = modify' (\(Game p im rs n itmap) -> Game p im rs newName itmap) >> showCurrentRoom
--gets (\(Game p im rs n itmap) -> rs M.! newName) >>= showRoom >> 
changePlayerParameter :: Name -> (Int -> Int) -> StIO ()
changePlayerParameter name f = modify' (\(Game (Player ps i lh rh) im rs n itmap) -> Game (Player (M.adjust f name ps) i lh rh) im rs n itmap)

getPlayerParameter :: Name -> StIO Int
getPlayerParameter name = gets (\(Game p im rs n itMap) -> playerParameters p M.! name)

changeEntityParameter :: Name -> Name -> (Int -> Int) -> StIO ()
changeEntityParameter entityName name f = modify' (\(Game p im rs n imap) -> Game p im rs n (M.adjust (\e -> Entity (getDescription e) (M.adjust f name $ entityParameters e) $ getCommands e) entityName imap))

getEntityParameter :: Name -> Name -> StIO Int
getEntityParameter entityName name = gets (\g -> (entityParameters $ globalNameMap g M.! entityName) M.! name)

conditionallyPerformAction :: StIO Bool -> StIO () -> StIO () -> StIO ()
conditionallyPerformAction cond trueaction falseaction = cond >>= (\c -> if c then trueaction else falseaction)

checkIfItemIsInInventory :: ItemName -> StIO Bool
checkIfItemIsInInventory name = gets (\g -> name `elem` (playerInventory . player) g)
