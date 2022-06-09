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
-- Hint for a list: if you want to leave list empty, simply write only its keyword.
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
-- It consists of its name and a code block (defined in next section). Command can be defined as silent,
-- so that it won't be available for a user. We define silent command by preceding its name with asterisk (@*@).
-- One command may have more than one name, simply define them separated by comma (without spaces!).
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
-- @
-- *silent:
--   {
--     # this is a silent command
--     (here are some instructions)
--   }
-- @
--
-- @
-- take,carry,tote:
--   {
--     # this is a command with multiple names
--     (here are some instructions)
--   }
-- @

-- *** Item
--
-- $item
--
-- Item is an object user can interact with. It is identified by its name. Its attributes are:
--
-- * @longName@ - the name shown in inventory
-- * @description@ - the switch showing an item description depending on given condiitons (e.g. item parameters)
-- * @parameters@ - YAML-like list of parameters
-- * @commands@ - YAML-like list of item commands. Commands for an item are invoked via its name and name of item after space, e.g. @throw axe@ or @punch dragon@.
--
-- Example:
--
-- @
-- bottle:
--   longName: A small bottle.
--   description:
--     - item.bottle.full == 1:
--         There is a bottle full of water.
--     - true:
--         There is an empty small bottle.
--   parameters:
--     - full:
--         value: 1
--   commands:
--     - drink:
--         {
--           println You drank all the water!
--           item.bottle.full = 0
--         }
-- @
--

-- *** Room
--
-- $room
--
-- Room is a place the player can stay and take actions. It is also identified by name and has the following attributes:
--
-- * @description@ - the switch showing a room description depending on given condiitons (e.g. some parameters)
-- * @onEntry@ (optional) - a code block which is run immediately after printing a description.
-- * @items@ - an item list initially present in a room.
-- * @commands@ - a command list for a given room. Room commands are invoked simply by their name.
--
-- Example:
--
-- @
-- kitchen:
--   description:
--     - true:
--         You are in the kitchen.
--   onEntry:
--     {
--       if has milk then
--       {
--         println Put that milk in a fridge! 
--       }
--     }
--   items:
--     - knife       -> only if defined earlier!
--     - fridge:
--         ...
--   commands:
--     - leave:
--         {
--           goto hall 
--         }
-- @
--

-- ** Code blocks
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
-- * @look@ - prints a room and items description (does not execute 'onEntry' of current room).
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

-- ** Expressions
--
-- $expressions
--
-- GAML provides following type of expressions:
--
-- * a number ('Int')
-- * @rnd@ - random number
-- * @invcount@ - current inventory count
-- * @cmdcount@ - number of commands entered by user, also incorrect ones
-- * @player.\<parameter\>@ - value of specified player's parameter
-- * @item.\<name\>.\<parameter\>@ - value of specified item parameter
--
-- There are also defined operators @+@, @-@, @*@, @/@ and @%@. The first two operators have lower priority, 
-- all operators are left-associative.
--

-- ** Boolean conditions
--
-- $conditions
--
-- GAML provides following types of conditions:
--
-- * @true@ (or @True@ or @TRUE@)
-- * @false@ (or @False@ or @FALSE@)
-- * comparison operators between arithmetic expressions: @==@, @!=@, @>@, @<@, @>=@ and @<=@
-- * @has \<item\>@ - true if specified item is in inventory
-- * @present \<item\>@ - true if specified item is in current room or in inventory
-- * @present \<item\> in \<room\>@ - true if specified item is in given room
-- * @askYesNo@ - prompts for an input, true if user enters @yes@ or @y@ (case insensitive), otherwise false
-- * @prompt \<word\>@ - prompts for an input, true if user enters the specified word, otherwise false
--
-- There are also defined logical operators: @&&@ (and) and @||@ (or). They have the same priority and are right-associative.
--

-- ** GAML structure
--
-- $structure
--
-- The structure of a GAML file consists of the following sections (in exactly that order):
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
-- Inventory is defined as a list of items.
--
-- Example:
--
-- @
-- player:
--   parameters:
--     - life:
--         value: 100
--   inventory:
--     - coin:
--         ...
-- @
--

-- *** Initial message
--
-- $initialMessage
--
-- Initial message is a multiline text printed at a startup.
--
-- Example:
--
-- @
-- initialMessage:
--   Welcome!
--   Would you like to see instructions?
-- @
--

-- *** Final message
--
-- $finalMessage
--
-- Final message is a switch, which can print different texts depending on e.g. game scenario.
--
-- Example:
--
-- @
-- finalMessage:
--   - player.life > 0
--       You win!
--   - true:
--       You lose.
--       Would you like to try again?
-- @
--

-- *** Items
--
-- $items
--
-- This section allows to define items which are not intended to be initially put in any room. It is a regular item list.
-- Remember that you cannot define item with the same name twice.
--
-- Example:
--
-- @
-- items:
--   - knife:
--       ...
-- @
--

-- *** Global commands
--
-- $globalCommands
--
-- In this section we can define:
--
-- * global 'onEntry' command (optional), which will be executed every time we go to any room, for which 'onEntry' is not defined
-- * global room commands, which will match for every room (e.g. @quit@ or @inv@)
-- * global item commands, which will match for every item (e.g. @take@)
--
-- Every global command can be overriden in room or item definition.
--
-- Example:
--
-- @
-- global:
--   onEntry:
--     {
--       ...
--     }
--   room:
--     - quit:
--        {
--          quit
--        }
--     ...
--   item:
--     - take:
--         {
--           if has then
--           {
--             take  
--           }
--           else
--           {
--             println You already have it!
--           }
--         }
--     ...
-- @
--

-- *** Rooms
--
-- $rooms
--
-- This section defines list of rooms used in game. Note that room may be used not only for interaction with user.
--
-- Example:
--
-- @
-- rooms:
--   - startRoom:
--       ...
--   ...
-- @
--

-- *** Start room name
--
-- $startRoomName
--
-- Here you have to specify the room which will be initial for a game.
--
-- Example:
--
-- @
-- start: garden
-- @
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

