module Commands where

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import DataStructures
import System.IO

noAction :: StIO ()
noAction = lift $ return ()

printMessage :: String -> StIO ()
printMessage s = lift $ putStr s >> hFlush stdout

printMessageLine :: String -> StIO ()
printMessageLine = lift . putStrLn

printInt :: Int -> StIO ()
printInt x = lift $ (putStr $ show x) >> hFlush stdout

goToRoom :: Name -> StIO ()
goToRoom newName = gets (\(Game p im rs n) -> show $ rs M.! n) >>= printMessage >> modify' (\(Game p im rs n) -> Game p im rs newName)

changePlayerParameter :: Name -> (Int -> Int) -> StIO ()
changePlayerParameter name f = modify' (\(Game (Player ps i lh rh) im rs n) -> Game (Player (M.adjust f name ps) i lh rh) im rs n)

getPlayerParameter :: Name -> StIO Int
getPlayerParameter name = gets (\(Game p im rs n) -> playerParameters p M.! name)

conditionallyPerformAction :: StIO Bool -> StIO () -> StIO () -> StIO ()
conditionallyPerformAction cond trueaction falseaction = cond >>= (\c -> if c then trueaction else falseaction)
