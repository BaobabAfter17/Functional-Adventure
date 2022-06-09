module Command where

import Text.Parsec hiding (parse, runParser, (<|>))
import qualified Text.Parsec as P 
import Text.Parsec.String (Parser)

import Data.Char
import Data.List
import Control.Monad (join)

import Item
import Direction

parse :: Parser a -> String -> Either ParseError a 
parse prsr = P.parse prsr ""

(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (try prsr1) prsr2

data Command
  = Inventory
  | Look
  | Drop [ItemName]
  | Take [ItemName]
  | Move Direction
  | Map
  | Exit
  deriving (Eq, Show)

type Conjunction = [Command]


-- 1.5 
itemNameP :: Parser ItemName
-- parse any string representing an ItemName into
-- an inhabitant of the ItemName datatype
itemNameP = foldl (<|>) (parserFail "")
              $ fmap singleItemP itemNames

singleItemP :: ItemName -> Parser ItemName
-- take an itemName and return a parser on itemName
singleItemP iname = pure iname
                    <* optional spacesP
                    <* (string $ show iname)
                    <* optional spacesP


-- 1.6.1
nounPhrase_stub :: Parser [ItemName]
-- a parser that takes an alphabetic string off the front
-- of the input, then puts it into a singleton list.
nounPhrase_stub = fmap (\x -> [x]) itemNameP

-- 2.0.8
nounPhrase :: Parser [ItemName]
-- parse a comma-separated list of nouns.
-- There can be an arbitrary amount of whitespace between
-- each noun and each comma.
nounPhrase = fmap join $ sepBy1 nounPhrase_stub (char ',')


-- 1.6.2
inventoryP :: Parser Command
-- accepts the string 'inventory' and rejects everything else.
-- It ignores whitespace on either side of the word 'inventory',
-- if there is any.
inventoryP = pure Inventory
             <* string "inventory" <|> string "i"


-- 1.6.3
takeP :: Parser Command
-- parses the word 'take' plus a noun phrase into a Command.
takeP = pure Take
        <* string "take" <|> string "t"
        <* spacesP
        <*> nounPhrase

spacesP :: Parser String
-- parse man1 whitespaces
spacesP = many1 $ char ' '


-- 1.6.4
exitP :: Parser Command
--  accept either the single word 'quit' or the single word
-- 'exit', and it will allow any amount of whitespace on
-- either side of either word
exitP = pure Exit
        <* string "exit" <|> string "quit" <|> string "q"


-- 2.0.1
dropP :: Parser Command
-- parses the word 'drop' plus a noun phrase into a Command.
dropP = pure Drop
        <* string "drop" <|> string "d"
        <* spacesP
        <*> nounPhrase


-- 2.0.2
lookP :: Parser Command
-- It only accepts the string 'look',
-- and it ignores whitespace on either side of the word 'look',
-- if there is any
lookP = pure Look
        <* string "look" <|> string "l"


-- 2.0.3
directionP :: Parser Direction
-- expects a single lowercase word denoting a direction,
-- making the obvious map from the word 'north', 'south',
-- 'east', or 'west' to the relevant Direction.
-- It allows for whitespace on either side, and does not
-- require the string to end right after.
directionP = foldl (<|>) (parserFail "")
              $ fmap singleDirP directions

singleDirP :: Direction -> Parser Direction
-- take an itemName and return a parser on itemName
singleDirP dir = pure dir
                 <* string (show dir)
                 <|> string ([head $ show dir])


-- 2.0.4
moveP :: Parser Command
-- expects one of the four words, 'north', 'south', 'east'
-- or 'west' (with possible but not obligatory whitespace
-- on either side). It consumes the relevant word off the
-- input, and makes that word into a command telling the
-- game to move in the relevant direction
moveP = pure Move <*> directionP


mapP :: Parser Command
-- It only accepts the string 'map',
-- and it ignores whitespace on either side of the word 'map',
-- if there is any
mapP = pure Map
       <* string "map" <|> string "m"


-- 2.0.5
commandP :: Parser Command
-- accepts any single command that is syntactically well-formed,
-- according to the grammar of the game's language, and returns
-- the Command corresponding to the string in the language. 
commandP = optional spacesP
           *> inventoryP
           <|> lookP
           <|> dropP
           <|> takeP
           <|> moveP
           <|> mapP
           <|> exitP
           <* optional spacesP


-- 2.0.6
conjunctionP :: Parser Conjunction
-- parses a list of commands, separated by the word 'and',
-- into a Conjunction
conjunctionP = sepBy1 commandP (string "and") <* eof


-- 2.0.7
parseInput :: String -> Maybe Conjunction
-- takes a string in and returns the conjunction wrapped in a Just,
-- if the input string is well-formed according to the game
-- language's syntax, and otherwise returns Nothing
parseInput str = case parse conjunctionP str of
                  Left _ -> Nothing
                  Right cmds -> Just cmds