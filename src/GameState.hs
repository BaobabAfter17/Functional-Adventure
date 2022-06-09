module GameState where

import Data.List
import Data.Char
import Control.Exception
import qualified Data.Map as M

import Item
import Room
import Player
import Direction

-- 1.4.1 Type alias for GameMap
type GameMap = M.Map RoomName Room
type Point = (Int, Int)
type GameGrid = M.Map Point Room

gameGrid :: GameGrid
-- The list below must be grouped by rows as required by 
-- showGridList
gameGrid = M.fromList [
        ((0, 0), bookshop)
        , ((0, 1), robeshop)
        , ((0, 2), broomstix)
        , ((1, 0), pub)
        , ((1, 1), wandshop)
        , ((1, 2), prophet)
        , ((2, 0), bank)
        , ((2, 1), owlshop)
    ]

mkGrid :: [Room] -> GameGrid
-- takes an input list of rooms and builds a square grid
mkGrid rooms =
  M.fromList $ map indexToEntry [0..length rooms - 1]
  where
    rowSize = ceiling . sqrt . fromIntegral . length $ rooms
    indexToEntry index = (
            (index `div` rowSize, index `mod` rowSize)
            , rooms !! index
        )



-- 1.5.1 Define GameStae
data GameState = GameState {
    message :: Maybe String
    , gmap :: GameMap
    , universe :: Universe
    , player :: Player
    , grid :: GameGrid
} deriving Show

-- 1.5.2 mkMap
-- similar like mkUniverse
mkMap :: [Room] -> GameMap
mkMap rooms =
    M.fromList nameItems
    where
        nameItems = map convert rooms
        convert room = (rname room, room)

-- 3.2.1 gameMap is a map if roomName Room
gameMap :: GameMap
gameMap = mkMap allRooms

-- 3.2.3 define initialState
initialState :: GameState
initialState = GameState {
        message = Nothing
        , gmap = gameMap
        , universe = univ
        , player = you
        , grid = gameGrid
    }

-- 3.3 
data KeyError = KeyError
  deriving Show

instance Exception KeyError

getObjectUniv :: ItemName -> Universe -> Item
getObjectUniv iname u
  = case M.lookup iname u of
      Just obj -> obj
      Nothing -> throw KeyError

getObject :: ItemName -> GameState -> Item
getObject iname st = getObjectUniv iname (universe st)

getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp
  = case M.lookup rname mp of
      Just room -> room
      Nothing -> throw KeyError

getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)

-- 3.3.1 setRoomMap
-- replace old room in gameMap with a new room 
setRoomMap :: RoomName -> Room -> GameMap -> GameMap
setRoomMap roomName room gm =
    M.adjust (\_ -> room) roomName gm

-- 3.3.2 setMessage
-- replace old message in gamestate to a new one
setMessage :: String -> GameState -> GameState
setMessage newString gameState =
    gameState {message = newMessage}
    where
        newMessage = if newString == ""
                    then Nothing 
                    else Just newString

-- 3.3.3 currentInventory
-- get inventory of player in gameState
currentInventory :: GameState -> [ItemName]
currentInventory gameState = inventory . player $ gameState

-- 3.3.4 currentRoom
-- takes a GameState as an input, returns whatever room
-- the Player is located at in that game state
currentRoom :: GameState -> Room
currentRoom gameState =
    getRoom rname gameState
    where
        rname = location $ player gameState

-- 3.3.5 nearbyObjects
-- takes a game state as input and returns the list of item
-- names in the room where the player is, in that game state.
nearbyObjects :: GameState -> [ItemName]
nearbyObjects gameState =
    objects $ currentRoom gameState

-- 3.2 1 takeItem of HW5
-- place various checks before actually taking an item
-- if something is wrong, just return the original state
takeItem :: ItemName -> GameState -> GameState
takeItem itemName gameState = case checkState of
    Left errorMessage -> gameState {message = Just errorMessage}
    Right _ -> takeItemWithoutCheck itemName gameState
    where checkState = 
            Right gameState
            >>= alreadyHaveTakeCheck itemName
            >>= inRoomTakeCheck itemName
            >>= weightCheck itemName

takeItemWithoutCheck :: ItemName -> GameState -> GameState
-- 3.3.6 takeItem
-- It takes an ItemName and a GameState as an input and returns
-- a new GameState with that ItemName removed from the room's
-- objects with that ItemName added to the GameState's 
-- Player's inventory, and with a message in the game state 
-- saying 'You take the ITEMNAME.' where ITEMNAME is the input
-- item name.
takeItemWithoutCheck itemName gameState =
    gameState {
        message = newMessage
        , player = newPlayer
        , gmap = newGameMap
    }
    where
        newPlayer =
            Player.addItem itemName $ player gameState
        newRoom =
            Room.removeItem itemName $ currentRoom gameState
        newGameMap =
            setRoomMap (rname newRoom) newRoom $ gmap gameState
        newMessage =
            Just $ "You take the " ++ (show itemName) ++ "."

dropItem :: ItemName -> GameState -> GameState
-- 3.2.2 HW5 place various checks before drop an item
-- if something wrong, just return the original state
dropItem itemName gameState = case checkState of
    Left errorMessage -> gameState {message = Just errorMessage}
    Right _ -> dropItemWithoutCheck itemName gameState
    where checkState =
            Right gameState
            >>= anywhereDropCheck itemName
            >>= inRoomDropCheck itemName

-- 3.3.7 dropItem
-- takes an ItemName and a GameState as an input, and returns 
-- a new GameState with that ItemName added to the room's 
-- objects, with that ItemName deleted from the GameState's
-- Player's inventory, and with a message in the game state 
-- saying 'You drop the ITEMNAME.' where ITEMNAME is the 
-- input item name
dropItemWithoutCheck :: ItemName -> GameState -> GameState
dropItemWithoutCheck itemName gameState =
    gameState {
        message = newMessage
        , player = newPlayer
        , gmap = newGameMap
    }
    where
        newPlayer = Player.removeItem itemName
            $ player gameState
        newRoom = Room.addItem itemName
            $ currentRoom gameState
        newGameMap = setRoomMap (rname newRoom) newRoom
            $ gmap gameState
        newMessage = Just
            $ "You drop the " ++ (show itemName) ++ "."

-- 3.0.1 inventoryWeight
inventoryWeight :: GameState -> Integer
-- return inventoryWeight of the player in the GameState
inventoryWeight gameState =
    foldl (\acc item -> acc + item) 0 itemWeights
    where
        itemWeights = map weight $ items
        items = map (\key -> univ M.! key) itemNames
        itemNames = inventory $ player gameState

-- 3.1 Error Handling with Either
type Error a = Either String a


-- 3.1.1 alreadyHaveTakeCheck
alreadyHaveTakeCheck :: ItemName
    -> GameState -> Error GameState
-- check wether the player in the gameState is carrying
-- such item if yes, return Left value saying the player is 
-- carrying, if not, return Right value, which is the gameState
alreadyHaveTakeCheck itemName gameState =
    if itemName `elem` (inventory $ player gameState)
    then Left $ unwords [
            "You are already carrying the"
            , (show itemName) ++ "."
        ]
    else Right gameState


-- 3.1.2 inRoomTakeCheck
inRoomTakeCheck :: ItemName -> GameState -> Error GameState
-- check wehtehr the player is nearby one item nearby means
-- the item is in the same room with player return Left error
-- message if not, and Right GameState if yes
inRoomTakeCheck itemName gameState = 
    if itemName `elem` nearbyObjects gameState
    then Right gameState
    else Left $ unwords [
            "There is no"
            , (show itemName)
            , "in this room."
        ]


-- 3.1.3 weightCheck
weightCheck :: ItemName -> GameState -> Error GameState
-- check if the player is able to carry a new item, based on 
-- weight of what they are carrying return Left value if 
-- total weight of everything the player is carrying plus 
-- current item exceeds max weight, return Right value 
-- otherwise.
weightCheck itemName gameState =
    if newItemWeight + inventoryWeight gameState
        > (maxWeight $ player gameState)
    then Left "That's too much weight for you to carry."
    else Right gameState
    where newItemWeight =
            weight $ getObject itemName gameState

-- 3.1.4 anywhereDropCheck
anywhereDropCheck :: ItemName -> GameState -> Error GameState
-- check wether the item is either in player inventory or 
-- in current room
anywhereDropCheck itemName gameState =
    if (elem itemName $ inventory $ player gameState)
        || (elem itemName $ objects $ currentRoom gameState)
    then Right gameState
    else Left $ unwords [
            "What do you mean, drop the",
            (show itemName) ++ "?"
        ]


-- 3.1.5 inRoomDropCheck
inRoomDropCheck :: ItemName -> GameState -> Error GameState
-- check whether the item is in the current room
inRoomDropCheck itemName gameState =
    if elem itemName $ objects $ currentRoom gameState
    then Left $ unwords [
            "You aren't carrying the",
            (show itemName) ++ "."
        ]
    else Right gameState

-- 3.3.1 hasObjects
hasObjects :: Room -> Bool
-- check if the room has objects
hasObjects room = not $ null $ objects room

-- 3.3.2 roomHasObjects
roomHasObjects :: GameState -> Bool
-- takes a GameState as input and tells you whether there
-- are any objects in the room where the Player in that
-- GameState is.
roomHasObjects gameState = hasObjects $ currentRoom gameState

-- 3.3.3 destinationName
destinationName :: Direction -> Room -> Maybe RoomName
-- takes a Direction and a Room as inputs, and if there
-- is an exit in that direction in the input room,
-- it outputs the name of the room that you would end up in,
-- otherwise return Nothing
destinationName dir room =
    foldl update Nothing $ exits room
    where
        update acc (d, rname) =
            if d == dir then Just rname else acc


-- 3.3.4 move
move :: Direction -> GameState -> GameState
-- move takes a Direction and GameState as inputs, and if it
-- is possible for the player to move in that direction,
-- returns a new game state with the player in the new location, 
-- and a message field containing Just "You go ...", 
-- where ... is the direction the player moves in to get to 
-- the new location. If it is not possible for the player to 
-- move in that direction, it returns the input game state, 
-- except with the message field changed to Just "There is
-- no exit in that direction.".
move dir gameState =
    case destinationName dir $ currentRoom gameState of
        Just rname -> setMessage
            ("You go " ++ (show dir) ++ ".")
            gameState {player = newPlayer}
            where
                newPlayer = newLocation rname $ 
                    player gameState
        Nothing -> setMessage
            "There is no exit in that direction."
            gameState


-- 3.4 haveWonGame
haveWonGame :: GameState -> Bool
-- returns True if the Player has collected all the following
-- items book, robe, nimbus, wand, owl
haveWonGame gameState = all collected items
    where
        collected item =
            item `elem` (currentInventory gameState)
        items = [Wand, Robe, Stone]


displayExit :: Direction -> Room -> String
-- takes a room and direction as an input and provides a
-- string representation of the exit in that direction,
-- if it exists, and otherwise provides a string consisting 
-- of a space. It should map a northerly or southerly exit
-- to "|", an easterly or westerly exit to "-",
-- and a non-exit to " ".
displayExit dir room =
    if elem dir $ map (\tuple -> fst tuple) $ exits room
    then case dir of
              N -> "|"
              S -> "|"
              W -> "-"
              E -> "-"
    else " "


roomLetter :: Maybe RoomName -> Room -> String
-- When you pass it Nothing, it'll just display the room
-- in the standard way. When you pass it e.g. Just Kitchen,
-- it will display the input room in the normal way,
-- unless that room is the kitchen, i.e. unless that room
-- has Kitchen for a room name.
roomLetter Nothing room = standardLetter room
roomLetter (Just roomName) room = 
    if roomName == rname room
    then "*"
    else standardLetter room

standardLetter :: Room -> String
-- Show the first letter of its RoomName capitalized.
standardLetter room =
    (toUpper $ head $ show $ rname room) : []


showGridList :: GameGrid -> [[Room]]
--  break a GameGrid up into a list of lists, where each
-- list represents a row of rooms in the game grid.
showGridList gameGrid = 
    map (map snd)
    . groupBy sameRow
    . sortOn fst
    . M.toList $ gameGrid
    where
        sameRow ((x1, _), _) ((x2, _), _) = x1 == x2


showRow :: Maybe RoomName -> [Room] -> String
-- take a list of rooms as an input and return
-- a three-line string
showRow maybeRoomName roomList =
    " " ++ fstLine ++ "\n" 
    ++ sndLine ++ "\n" ++ " "
    ++ trdLine ++ "\n"
    where
        fstLine =
            intercalate "  " $ map (displayExit N) roomList
        sndLine = intercalate "" $ map showEWExit roomList
        trdLine =
            intercalate "  " $ map (displayExit S) roomList
        showEWExit room =
            (displayExit W room)
            ++ (roomLetter maybeRoomName room)
            ++ (displayExit E room)

showGrid :: Maybe RoomName -> GameGrid -> String
-- takes an optional room name indicating which room (if any)
-- should be marked with a star and a game grid as inputs,
-- and outputs a string representation of the grid.
showGrid maybeRoomName gameGrid =
    intercalate ""
    $ map (showRow maybeRoomName)
    $ showGridList gameGrid