{-# LANGUAGE OverloadedStrings #-}

module Parsers.EntityParser where

import DataStructures
import qualified Data.Map.Strict as M
import Data.Attoparsec.Text
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Text
import Data.Char
import Commands
import Control.Applicative
import Data.Maybe
import Parsers.Utilities
import Parsers.CommandParser
import Parsers.CodeParser
import Parsers.ParametersParser
import Parsers.SwitchParser

modifyNameMapIfNeeded :: Name -> StParser Name
modifyNameMapIfNeeded name = 
    gets ((M.member name) . fst) 
    >>= (\exists -> if exists
        then lift (fail "An item or entity with provided name already exists!")
        else lift (Entity 
                <$> (char ':' *> newLines *> tabs 6 *> switchParser "description" 6 "Entity description")
                <*> (parametersParser 6) 
                <*> (tabs 6 *> commandListParser 7 codeParser <* newLines))
            >>= (\item -> modify' (\(m,a) -> (M.insert name item m,a)))
            >> (lift $ pure name))

entityParser :: StParser Name
entityParser = 
    (lift (unpack <$> takeWhile1 isAlphaNum) >>= modifyNameMapIfNeeded)
    <??> "Entity definition" --Entity
--     <$> (tabs 3 *> (unpack <$> takeWhile1 isAlphaNum))
--     <*> parametersParser 4
--     <*> (char ':' *> newLines *> tabs 3 *> commandListParser 4 codeParser <* newLines)

entityListParser :: StParser [Name]
entityListParser = listParserSt "entities" entityParser 4 "Entity list definition"

test :: Result Name
test = feed (parse (evalStateT entityParser (M.empty,noAction)) 
    "dragon:\n\
    \            parameters:\n\
    \            commands:\n"
    ) ""