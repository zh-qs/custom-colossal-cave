-- |The startup module for application. Contains entry function 'main' and functions to read arguments from command line.
module Main where

import Data.List
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import System.Environment
import System.IO.Error
import GameParser
import GamePlay

-- |The 'String' which is displayed in case of incorrect number of arguments provided.
usage :: String
usage = "USAGE: the-game game-file.game"

-- |Gets file name from arguments: if number of arguments provided is not equal to one, return 'Left usage'.
getFileNameFromArgs :: [String] -> IO (Either String String)
getFileNameFromArgs (file:[]) = return $ Right file
getFileNameFromArgs _ = return $ Left usage

-- |Execute a 'StateT' transformer, but return nothing.
execStateT_ :: Monad m => StateT s m a -> s -> m ()
execStateT_ st s = void $ execStateT st s

-- |The entry point of the application.
main :: IO ()
main = do
  eitherFile <- getArgs >>= getFileNameFromArgs
  eitherGame <- either (\msg -> return $ Left msg) (\file -> parseGameFromFile file) eitherFile
  either (\msg -> putStrLn msg) (\game -> execStateT_ mainStart game) eitherGame

-- |Parse a file from 'FilePath' and run a game.
ghciMain :: FilePath -> IO ()
ghciMain path = do
  eitherGame <- parseGameFromFile path
  either (\msg -> putStrLn msg) (\game -> execStateT_ mainStart game) eitherGame

