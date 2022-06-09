module Room where

import Item
import Direction
import Data.List

data RoomName
  = Bookshop
  | Robeshop
  | Broomstix
  | Wandshop
  | Owlshop
  | Pub
  | Prophet
  | Bank
  deriving (Eq, Ord)

instance Show RoomName where
    show rname = case rname of
        Bookshop -> "Flourish & Blotts Bookshop"
        Robeshop -> "Madam Malkin's Robes"
        Broomstix -> "Broomstix"
        Wandshop -> "Ollivanders"
        Owlshop -> "Eeylop's Owl Emporium"
        Pub -> "Leaky Cauldron"
        Prophet -> "Daily Prophet"
        Bank -> "Gringotts Wizarding Bank"

type Exit = (Direction, RoomName)

data Room =
    Room { rname :: RoomName
        , desc :: String
        , exits :: [Exit]
        , objects :: [ItemName]}
    deriving(Show, Eq)


-- 3.1.2 Rooms
bookshop :: Room
bookshop = Room {
        rname = Bookshop
        , desc = "Today is the 973th day since the manager tried to find The Invisible Book of Invisibility, and The Monster Book of Monsters are tearing each other up."
        , exits = [(E, Robeshop)]
        , objects = [Book, Parchment, Quill]
    }        

robeshop :: Room
robeshop = Room {
        rname = Robeshop
        , desc = "School uniforms! Dress robes! Travelling cloaks! aaand invisibility cloaks..."
        , exits = [(W, Bookshop), (E, Broomstix), (S, Owlshop)]
        , objects = [Robe, Hat, Pouch]
    }
    
broomstix :: Room
broomstix = Room {
        rname = Broomstix
        , desc = "As every school-age wizard knows, the fact that we fly on broomsticks is probably our worst-kept secret."
        , exits = [(W, Robeshop)]
        , objects = [Nimbus, FireBolt, ThunderBolt]
    }

pub :: Room
pub = Room {
        rname = Pub
        , desc = "A gateway between the non-wizarding world and Diagon Alley."
        , exits = [(S, Bank), (E, Wandshop)]
        , objects = [ButterBeer, FireWhiskey]
    }

wandshop :: Room
wandshop = Room {
        rname = Wandshop
        , desc = "Makers of Fine Wands since 382 B.C."
        , exits = [(W, Pub), (E, Prophet), (N, Robeshop)]
        , objects = [Wand]
    }

prophet :: Room
prophet = Room {
        rname = Prophet
        , desc = "A publishing house affiliated with the Daily Prophet. Among their range of titles was The Life and Lies of Albus Dumbledore, by Rita Skeeter."
        , exits = [(W, Wandshop)]
        , objects = [Newspaper]
    }

bank :: Room
bank = Room {
        rname = Bank
        , desc = "Gringotts is the safest place in the world fer anything yeh want ter keep safe -- 'cept maybe Hogwarts."
        , exits = [(N, Pub), (E, Owlshop)]
        , objects = [Knut, Sickle, Galleon, Goblin, Stone]
    }

owlshop :: Room
owlshop = Room {
        rname = Owlshop
        , desc = "A dark shop that sells owls."
        , exits = [(W, Pub)]
        , objects = [Owl, Toad, Mouse, Hamster]
    }

roomNames :: [RoomName]
-- 3.1.5 roomNames
-- 3.2.2 rewritten using allRooms
roomNames = map rname allRooms

-- 1.3.2 addItem
-- Add an item to the room
addItem :: ItemName -> Room -> Room
addItem itemName room =
  room {objects = itemName : (objects room)}

-- 1.3.3 removeItem
-- remove an item from the room
removeItem :: ItemName -> Room -> Room
removeItem itemName room =
  room {objects = delete itemName $ objects room}

-- 3.2.1 allRooms
allRooms :: [Room]
allRooms = [
        bookshop
        , robeshop
        , broomstix
        , wandshop
        , owlshop
        , pub
        , prophet
        , bank
    ]