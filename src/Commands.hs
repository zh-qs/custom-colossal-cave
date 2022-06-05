module Commands where

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import DataStructures
import Data.List
import System.IO
import Data.Maybe

showAndExecuteOnEntryCurrentRoom :: Action ()
showAndExecuteOnEntryCurrentRoom = Perform $ ((\g -> rooms g M.! currentRoomName g) <$> get)
    >>= (\r -> 
        description r >>= lift . putStrLn
        >> foldl' (\stio name -> stio >> gets (\g -> globalNameMap g M.! name) >>= getDescription >>= lift . putStr) (lift $ return ()) (interactables r)
        >> onEntry r)

noAction :: Action ()
noAction = Perform $ lift $ return ()

showInventory :: Action ()
showInventory = Perform $ gets (\g -> (playerInventory $ player g, globalNameMap g)) >>= (\(inv,imap) -> lift $ putStrLn $ foldl' (\s itName -> s ++ longName (imap M.! itName) ++ "\n") "Your inventory:\n" inv)

showFinalMessage :: Action ()
showFinalMessage = Perform $ gets finalMessage >>= id >>= lift . putStr

callCommandForCurrentRoom :: Name -> Action ()
callCommandForCurrentRoom name = Perform $ gets (\g -> fromJust $ lookup name $ map (\(_,n,r) -> (n,r)) $ roomCommands $ rooms g M.! currentRoomName g) >>= id

takeItem :: ItemName -> Action ()
takeItem item = Perform $ modify' (\(Game (Player ps i lh rh) im fm gr gi rs n itMap) -> 
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
giveItem item = Perform $ modify' (\(Game (Player ps i lh rh) im fm gr gi rs n itMap) -> 
    Game 
        (Player ps (item:i) lh rh) 
        im fm 
        gr gi
        rs 
        n 
        itMap)

putItem :: ItemName -> Action ()
putItem item = Perform $ modify' (\(Game p im fm gr gi rs n itMap) -> 
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
dropItem item = Perform $ modify' (\(Game (Player ps i lh rh) im fm gr gi rs n itMap) -> 
    Game 
        (Player ps (filter (/= item) i) lh rh) 
        im fm 
        gr gi
        (M.adjust 
            (\(Room d e its cmds) -> Room d e (item:its) cmds)
            n rs) 
        n 
        itMap)

discardItem :: ItemName -> Action ()
discardItem item = Perform $ modify' (\(Game (Player ps i lh rh) im fm gr gi rs n itMap) -> 
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
printMessage s = Perform $ lift $ putStr s >> hFlush stdout

printMessageLine :: String -> Action ()
printMessageLine = Perform . lift . putStrLn

printInt :: Int -> Action ()
printInt x = Perform $ lift $ (putStr $ show x) >> hFlush stdout

goToRoom :: Name -> Action ()
goToRoom newName = Perform $ modify' (\(Game p im fm gr gi rs n itmap) -> Game p im fm gr gi rs newName itmap) >> showAndExecuteOnEntryCurrentRoom
--gets (\(Game p im fm rs n itmap) -> rs M.! newName) >>= showRoom >> 
changePlayerParameter :: Name -> (Int -> Int) -> Action ()
changePlayerParameter name f = Perform $ modify' (\(Game (Player ps i lh rh) im fm gr gi rs n itmap) -> Game (Player (M.adjust f name ps) i lh rh) im fm gr gi rs n itmap)

getPlayerParameter :: Name -> Action Int
getPlayerParameter name = Perform $ gets (\(Game p im fm gr gi rs n itMap) -> playerParameters p M.! name)

changeEntityParameter :: Name -> Name -> (Int -> Int) -> Action ()
changeEntityParameter entityName name f = Perform $ modify' (\(Game p im fm gr gi rs n imap) -> Game p im fm gr gi rs n (M.adjust (\e -> Entity (getDescription e) (M.adjust f name $ entityParameters e) $ getCommands e) entityName imap))

getEntityParameter :: Name -> Name -> Action Int
getEntityParameter entityName name = Perform $ gets (\g -> (entityParameters $ globalNameMap g M.! entityName) M.! name)

conditionallyPerformAction :: Action Bool -> Action () -> Action () -> Action ()
conditionallyPerformAction cond trueaction falseaction = cond >>= (\c -> if c then trueaction else falseaction)

conditionallyEvaluateAction :: Action Bool -> (a -> Action ()) -> (a -> Action ()) -> a -> Action ()
conditionallyEvaluateAction cond ftrue ffalse a = conditionallyPerformAction cond (ftrue a) (ffalse a)

checkIfItemIsInInventory :: ItemName -> Action Bool
checkIfItemIsInInventory name = Perform $ gets (\g -> name `elem` (playerInventory . player) g)
