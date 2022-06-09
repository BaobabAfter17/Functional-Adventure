module Player where

import Item
import Room

import Data.List

data Player =
    Player { inventory :: [ItemName]
            , maxWeight :: Integer
            , location :: RoomName}
    deriving (Show, Eq)

addItem :: ItemName -> Player -> Player
-- add item to player's inventory
addItem item (Player inventory maxWeight location) =
    Player updatedInventory maxWeight location
    where
        updatedInventory = item : inventory

removeItem :: ItemName -> Player -> Player
-- remove item from player's inventory
removeItem item player =
    player {inventory = delete item $ inventory player}

newLocation :: RoomName -> Player -> Player
-- take a room name and a player as input, 
-- and return a new player whose location has been updated
-- to be that of the room named.
newLocation roomName player = player {location = roomName}

isCarryingAnything :: Player -> Bool
-- maps a player to True if that player's inventory
-- has something in it, and False otherwise.
isCarryingAnything player = not . null . inventory $ player

you :: Player
-- 3.1.6 you
you = Player [] 200 Pub