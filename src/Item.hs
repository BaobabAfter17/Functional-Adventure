module Item where

import qualified Data.Map as M

data ItemName
  = Book
  | Parchment
  | Quill
  | Robe
  | Hat
  | Pouch
  | Nimbus
  | FireBolt
  | ThunderBolt
  | Wand
  | Owl
  | Toad
  | Mouse
  | Hamster
  | ButterBeer
  | FireWhiskey
  | SingleMaltWhiskey
  | Newspaper
  | Knut
  | Sickle
  | Galleon
  | Goblin
  | Stone
  deriving (Eq, Ord)

instance Show ItemName where
-- rewrite show instance for ItemnName
  show iname = case iname of
    Book -> "The Monster Book of Monsters"
    Parchment -> "Marauder's Map"
    Quill -> "Black Quill"
    Robe -> "The Invisibility Cloak"
    Hat -> "Sorting Hat"
    Pouch -> "Mokeskin Pouch"
    Nimbus -> "Nimbus 2000"
    FireBolt -> "FireBolt"
    ThunderBolt -> "ThunderBolt"
    Wand -> "The Elder Wand" 
    Owl -> "Hedwig"
    Toad -> "Trevor"
    Mouse -> "Peter Pettigrew"
    Hamster -> "Zhicong Ma"
    ButterBeer -> "ButterBeer"
    FireWhiskey -> "FireWhiskey"
    Newspaper -> "The Daily Prophet"
    Knut -> "Knut"
    Sickle -> "Sickle"
    Galleon -> "Galleon"
    Goblin -> "GripHook"
    Stone -> "The Resurrection Stone"

data Item
    = Item { iname :: ItemName
            , weight :: Integer }
    deriving (Show, Eq)

type Universe = M.Map ItemName Item

mkUniverse :: [Item] -> Universe
-- take a list of items and construct a Universe 
-- (that is, a key-value store with ItemName-s
-- as keys and Item-s as values) from it
mkUniverse items =
  M.fromList $ map (\item -> (iname item, item)) items

book :: Item
book = Item Book 10

parchment :: Item
parchment = Item Parchment 1

quill :: Item
quill = Item Quill 3

robe :: Item
robe = Item Robe 50

hat :: Item
hat = Item Hat 20

pouch :: Item
pouch = Item Pouch 10

nimbus :: Item
nimbus = Item Nimbus 100

firebolt :: Item
firebolt = Item FireBolt 150

thunderbolt :: Item
thunderbolt = Item ThunderBolt 200

wand :: Item
wand = Item Wand 10

owl :: Item
owl = Item Owl 20

toad :: Item
toad = Item Toad 10

mouse :: Item
mouse = Item Mouse 5

hamseter :: Item
hamseter = Item Hamster 0

beer :: Item
beer = Item ButterBeer 10

whiskey :: Item
whiskey = Item FireWhiskey 10

paper :: Item
paper = Item Newspaper 1

knut :: Item
knut = Item Knut 5

sickle :: Item
sickle = Item Sickle 5

galleon :: Item
galleon = Item Galleon 10

goblin :: Item
goblin = Item Goblin 60

stone :: Item
stone = Item Stone 40

-- 3.1.3 Universe
itemList :: [Item]
itemList = [
    book
    , parchment
    , quill
    , robe
    , hat
    , pouch
    , nimbus
    , firebolt
    , thunderbolt
    , wand
    , owl
    , toad
    , mouse
    , hamseter
    , beer
    , whiskey
    , paper
    , knut
    , sickle
    , galleon
    , goblin
    , stone
  ]

univ :: Universe
univ = mkUniverse itemList

itemNames :: [ItemName]
-- 3.1.4 itemNames
itemNames = M.keys univ