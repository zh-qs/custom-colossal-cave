{-# LANGUAGE OverloadedStrings #-}

module Parsers.Utilities where

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
import Data.List

-- |Returns @True@ if 'Char' is a new line character (@\n@ or @\r@).
isNewline :: Char -> Bool
isNewline c = c == '\n' || c == '\r'

-- |Skip consecutive whitespace characters.
skipSpaces :: Parser ()
skipSpaces = skipWhile isSpace

-- |Match a char with whitespace characters around.
charWithSpaces :: Char -> Parser Char
charWithSpaces c = skipSpaces *> char c <* skipSpaces

-- |Match a string with whitespace characters around.
stringWithSpaces :: Text -> Parser Text
stringWithSpaces s = skipSpaces *> string s <* skipSpaces

-- |Match zero or more newline characters.
newLines :: Parser ()
newLines = (skipWhile isNewline) <?> "no newline"

-- |Match one or more newline characters.
newLines1 :: Parser ()
newLines1 = (skip isNewline *> skipWhile isNewline) <?> "no newline"

-- |Match a tab character (@\t@) or two spaces.
tabOrTwoSpaces :: Parser ()
tabOrTwoSpaces = ((void $ char '\t') <|> (void $ string "  ")) <?> "unproper indenting"

-- |Match a number of tabs (or two spaces) specified by 'Int'.
tabs :: Int -> Parser ()
tabs n = Prelude.foldr (*>) (return ()) (Prelude.take n $ repeat $ tabOrTwoSpaces)

-- |Match a list of items stated by 'Text'. 'Parser a' matches single item, 'Int' is an indentation level and 'String' is a message in case of parsing failure.
listParser :: Text -> Parser a -> Int -> String -> Parser [a]
listParser keyword parser indentationLevel msg = 
    (((string keyword *> char ':') <?> "keyword") 
    *> newLines  
    *> (many' (tabs indentationLevel *> string "- " *> parser)))
    <?> msg

-- |Match a list of items stated by 'Text'. Lifted version of 'listParser'.
listParserSt :: Text -> StParser a -> Int -> String -> StParser [a]
listParserSt keyword parser indentationLevel msg = 
    (lift ((string keyword *> char ':') <?> "keyword") 
    *> lift newLines  
    *> (many' (lift (tabs indentationLevel) *> lift (string "- ") *> parser)))
    <??> msg

-- |Match a code line with keyword 'Text', instruction content parser 'Parser a'. 'String' is a message in case of parsing failure.
baseCodeLineParser :: Text -> Parser a -> String -> Parser a
baseCodeLineParser cmd parser msg = ((string cmd <?> "keyword") *> parser <* newLines) <?> msg 

-- |Match a multiline string, where 'Text' is a keyword,'Int' is an indentation level and 'String' is a message in case of parsing failure.
multilineContentParser :: Text -> Int -> String -> Parser String
multilineContentParser keyword indentationLevel msg = (string keyword *> char ':' *> newLines *> (Data.List.foldl' <$> return (++) <*> return "" <*> many' (tabs (indentationLevel + 1) *> (((++"\n") . unpack) <$> Data.Attoparsec.Text.takeTill isNewline) <* newLines))) <?> msg

-- |Match a multiline string, 'Int' is an indentation level and 'String' is a message in case of parsing failure.
multilineContentParser' :: Int -> String -> Parser String
multilineContentParser' indentationLevel msg = (Data.List.foldl' <$> return (++) <*> return "" <*> many' (tabs (indentationLevel + 1) *> (((++"\n") . unpack) <$> Data.Attoparsec.Text.takeTill isNewline) <* newLines)) <?> msg

-- |Lifts a two-argument function to 'Action' and wraps it into a 'Parser'.
liftPAction :: (a -> a -> b) -> Parser (Action a -> Action a -> Action b)
liftPAction f = return (\ax ay -> f <$> ax <*> ay)

-- |Lifts a two-argument function to 'StIO' and wraps it into a 'Parser'.
liftPStIO :: (a -> a -> b) -> Parser (StIO a -> StIO a -> StIO b)
liftPStIO f = return (\stx sty -> f <$> stx <*> sty)

-- |Version of attoparsec's function 'eitherResult', which in case of failure prints additionally a piece of unconsumed GAML code.
eitherResult' :: Result r -> Either String r
eitherResult' (Done _ r)        = Right r
eitherResult' (Fail txt [] msg)   = Left (msg ++ " near: \n" ++ Data.List.take 100 (unpack txt))
eitherResult' (Fail txt ctxs msg) = Left (Data.List.intercalate " > " ctxs ++ ": " ++ msg ++ " near: \n" ++ Data.List.take 100 (unpack txt))
eitherResult' _                   = Left "Result: incomplete input"
