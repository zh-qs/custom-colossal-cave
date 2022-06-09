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

-- * The GAML language
--
-- $gaml
-- 
-- The GAML language was created to simplify text adventure games creation, which balances between maximal functionality and language simplicity.
-- It is based on a YAML language (where a tab is either a @\t@ char ot two spaces), with an addition of code blocks.
--

-- ** Types
--
-- $types
--
-- The following structures are defined in GAML:
--

-- *** Parameter
--
-- $parameter
--
-- The single parameter is defined with its name and value (of type 'Int'), for example:
--
-- @
-- life:
--   value: 100
-- @
--

-- *** Switch
--
-- $switch
--
-- Switch is used to print a text depending of a specified condition. It is represented
-- as a YAML list of boolean expressions and associated multiline texts. It is used in description
-- or printing final message.
--
-- Example (the @"description"@ header may vary):
--
-- @
-- description:
--   - player.life == 0:
--       You are dead.
--   - true:
--       Look around.
--       Isn't it beautiful?
-- @
--

-- *** Command
--
-- $command
--
-- The command represents an action which will be performed after user types its name in a console.
-- It consists of its name and a code block (defined below).
--
-- Example:
--
-- @
-- sleep:
--   {
--     (here are some instructions)
--   }
-- @
--

-- *** Code block
--
-- $codeBlock
--
-- A single code block consists of a list (but not YAML-like, without dashes!) of instructions.
-- Inside commands blocks indentation is not checked, as every instruction should be in the other line.
-- 
-- The possible instructions are:
--
-- * @goto \<room\>@ - moves the player to a room specified by name, prints its description and executes @onEntry@ block, if defined.
-- * @print.@ - prints an empty line. 
-- * @print \<text\>@ - prints a text, but doesn't end it with a newline character.
-- * @println \<text\>@ - prints a line if a text
-- * @printval \<expr\>@ - prints a value of specified arithmetic expression, e.g. @printval player.life + 1@
-- * @player.\<parameter\> =|+=|-=|*=|/=|%= \<expr\>@ - sets (or modifies) player parameter (a C-like instruction)
-- * @item.\<name\>.\<parameter\> =|+=|-=|*=|/=|%= \<expr\>@ - the same as above, but for an item.
-- * @give \<item\>@ - gives an item to a player (puts into an inventory)
-- * @take \<item\>@ - takes an item from a current room to an inventory
-- * @drop \<item\>@ - drops an item from an inventory in a current room
-- * @put \<item\> [in \<room\>]@ - puts an item in a specified room, if room name ommited, assumes the current room.
-- * @remove \<item\>@ - removes an item from a current room and from player's inventory.
-- * @remove \<item\> from \<room\>@ - removes an item from a specified room
-- * @move \<item\> to \<room\>@ - moves an item from current room to a specified room. If an item does not exist in current room, no action is performed.
-- * @call \<command\>@ - calls a specified command defined in current room. If not found, tries to find a global command with a specified name. If not found, terminates a game with an error.
-- * @global room.\<command\>@ - calls a global command for rooms. If not found, terminates a game with an error.
-- * @global item.\<command\> \<item\>@ - calls a global command for a specified item. If item or command not found, terminate a game with an error.
-- * @globalOnEntry@ - calls a global onEntry code. If not defined, performs no action.
-- * @quit@ - prints a final message and terminates the game.
-- * @save@ - prompts for a file name and saves game state to a file. The prompt is silent, it is suggested to use @print@ earlier.
-- * @restore@ - prompts for a file name and tries to restore game state from a file. In case of failure, no action is performed. As above, prompt is also silent.
-- * @showInventory@ - prints long names of items in player's inventory.
-- * @dropall@ - drops all items from inventory. Useful when player dies.
-- * @# \<comment\>@ - a comment
--
-- For commands defined for items, the additional instructions are defined:
--
-- * @take@ - take an item from current room to an inventory
-- * @drop@ - drop an item from an inventory to a current room
-- * @discard@ - remove an item
-- * @global item.\<command\>@ - calls a global command for item, in context of the item calling.
-- * @if has then {\<codeBlock\>} [else {\<codeBlock\>}]@ - conditionally executes action, depending of existence of an item in an inventory.
-- * @if present then {\<codeBlock\>} [else {\<codeBlock\>}]@ - conditionally executes action, depending of existence of an item in a current room or in inventory.
--
-- These two conditionals defined for items are the special cases, conditions @has@ and @present@ should not be used in regular boolean expressions.
-- The "true" conditional is defined as follows:
--
-- @
-- if \<condition\> then
-- {
--   \<codeBlock\>
-- }
-- [else if \<condition\> then
-- {
--   \<codeBlock\>
-- }]
-- ...
-- [else
-- {
--   \<codeBlock\>
-- }]
-- @
-- 

-- ** GAML structure
--
-- $structure
--
-- The structure of a GAML file consists of the following sections:
--

-- *** Messages in case of unknown command
--
-- $unknownMessages
-- 
-- This section is optional. It is used to define a list of single-line messages which will be printed if user types a command, which is not handled at a time.
-- The runtime prints a random message from a list. If this section is omitted, every time user types an unknown command, text @"Unknown command"@
-- will be printed.
--
-- Example:
-- 
-- @
-- unknownMessages:
--   - I don't know what you are saying.
--   - Could you repeat, please?
-- @
--

-- *** Player
-- 
-- $player
--
-- In this section we define player attributes: his parameters and initial values, and his initial inventory. 
--

-- ** Gameplay
--

-- * Documentation

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

