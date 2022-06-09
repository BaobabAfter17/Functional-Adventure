module Example where

import System.Random
import Data.Array.IO
import Control.Monad
import Data.List as L
import Data.Map as M
import Data.Maybe (fromJust)

import Item
import Direction
import Room
import Player
import GameState

choose :: [a] -> IO a
-- 3.2.1 randomly choose one item in list
choose lst = do
  index <- randomRIO(0, length lst - 1)
  return $ lst !! index

exampleList :: IO a -> IO Int -> IO [a]
-- 3.2.2 exampleList
exampleList eleIO numberIO = do
  number <- numberIO
  sequence $ replicate number eleIO

-- 3.2.3 Example type class
class Example a where
  example :: IO a

-- final project below
newtype Set = Set [Point] deriving (Show, Eq)


shuffle :: [a] -> IO [a]
-- randomly shuffling the elements of a list
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs


mkSet :: (Int, Int) -> Set
-- takes a point as an input and returns that same point 
-- enclosed in a singleton set. 
mkSet point = Set [point]


zeroExits :: Room -> Room
-- takes a room as an input and returns the same room with 
-- its exits deleted
zeroExits room = room {exits = []}


exampleGrid :: [Room] -> IO GameGrid
-- takes an input list of rooms and builds a square grid 
-- in which those rooms are randomly arranged.
exampleGrid rooms = do
  shuffledRooms <- shuffle rooms
  return $ mkGrid shuffledRooms


neighbors :: Point -> Point -> Bool
--  takes two grid points as inputs and returns True if those
--  two points are next to each other either vertically or 
-- horizontally.
neighbors (x1, y1) (x2, y2) =
  x1 == x2 && abs (y1 - y2) == 1
  || y1 == y2 && abs (x1 - x2) == 1


possibleLinks :: Set -> Set -> [(Point, Point)]
-- takes two sets of points as inputs and returns a list of 
-- all pairs of points, one from the first set and the other 
-- from the second set, that are neighbors of each other.
possibleLinks (Set ps1) (Set ps2) =
  [(p1, p2) | p1 <- ps1, p2 <- ps2, neighbors p1 p2]


orientation :: Point -> Point -> Maybe Direction
-- takes two grid points as an input, and returns the direction 
-- corresponding to the direction moving from the first grid 
-- point to the second grid point, wrapped in a Just.
orientation (x1, y1) (x2, y2)
  | x2 == x1 && y2 > y1 = Just E
  | x2 == x1 && y2 < y1 = Just W
  | y2 == y1 && x2 > x1 = Just S
  | y2 == y1 && x2 < x1 = Just N
  | otherwise = Nothing


addExit :: Room -> Direction -> Room -> Room
-- takes a destination room, a direction, and a source room, 
-- and adds an exit going in the input direction to the source 
-- room's list of exits.
addExit fromRoom dir toRoom = 
  if any (\(d, _) -> d == dir) $ exits fromRoom
  then fromRoom
  else fromRoom {
      exits = (dir, rname toRoom) : (exits fromRoom)
    }
  

nodeLink :: Point -> Point -> GameGrid -> Maybe GameGrid
-- takes two points and a game grid as inputs, and outputs 
-- a new game grid that is the result of linking the exits
-- in the rooms located at those two points vertically if 
-- they're vertically aligned, or horizontally if they're 
-- horizontally aligned, wrapped in a Just. If they are 
-- neither vertically nor horizontally aligned, it returns 
-- Nothing. If either of the input points are not on the grid,
-- it returns Nothing.
nodeLink p1 p2 gameGrid =
  if notMember p1 gameGrid 
    || notMember p2 gameGrid 
    || not (neighbors p1 p2)
  then Nothing
  else Just gameGrid 
        >>= addExitInGrid p1 p2
        >>= addExitInGrid p2 p1


addExitInGrid :: Point -> Point -> GameGrid -> Maybe GameGrid
-- add Exit to room at p1, the exit goes to room at p2
-- p1, p2 must be neighbors and both be contained in gameGrid
addExitInGrid p1 p2 gameGrid
  | maybeR1 == Nothing || maybeR2 == Nothing = Nothing
  | otherwise = Just $ M.adjust (\_ -> newr1) p1 gameGrid
      where
        maybeR1 = M.lookup p1 gameGrid
        maybeR2 = M.lookup p2 gameGrid
        dir = fromJust $ orientation p1 p2
        newr1 = addExit (fromJust maybeR1) dir (fromJust maybeR2)


setNeighbors :: Set -> Set -> Bool
-- takes two Set-s as an input, and tells you whether 
-- there are any neighboring points, the first from the first 
-- set and the second from the second set.
setNeighbors (Set ps1) (Set ps2) =
  any id [neighbors p1 p2 | p1 <- ps1, p2 <- ps2]


setUnion :: Set -> Set -> Set
-- It takes two sets of points as inputs and returns their union.
setUnion (Set ps1) (Set ps2) = Set $ L.union ps1 ps2


setLink :: Set -> Set -> GameGrid -> IO (Maybe GameGrid)
-- takes two sets of points and a game grid as inputs, and 
-- returns a game grid with two exits linking rooms located at
-- randomly-selected pair of neighboring points, one from the 
-- first set and one from the second set.
setLink s1 s2 gameGrid
  | setNeighbors s1 s2 = do
      (p1, p2) <- choose $ possibleLinks s1 s2
      return $ nodeLink p1 p2 gameGrid
  | otherwise = return $ Nothing


updateSetList :: Set -> Set -> [Set] -> [Set]
-- takes two sets and a list of sets as an input. You can 
-- assume that the input list of sets will contain each of 
-- the two input sets, possibly among others. It deletes the 
-- two input sets from the list of sets, and adds the union 
-- of those two input sets to the list.
updateSetList s1 s2 ss =
  (setUnion s1 s2) : (L.delete s1 $ L.delete s2 ss)


oneRound :: [Set] -> GameGrid -> IO (Maybe GameGrid, Set, Set)
-- performs a single round of Kruskal's algorithm.
oneRound ss gameGrid = do
  (s1, s2) <- choose [
      (s1, s2)
      | s1 <- ss, s2 <- ss
      , s1 /= s2 && setNeighbors s1 s2
    ]
  maybeGrid <- setLink s1 s2 gameGrid
  return $ (maybeGrid, s1, s2)


generateMap :: [Set] -> GameGrid -> IO GameGrid
-- takes an initial list of sets and keeps performing oneRound,
-- drawing a link between a random pair of neighboring rooms 
-- that occupy a randomly chosen pair of neighboring sets, 
-- until there is only one set left in the list. 
generateMap ss gameGrid
  | length ss == 1 = return gameGrid
  | otherwise =
    do
      (maybeGrid, s1, s2) <- oneRound ss gameGrid
      case maybeGrid of
        Nothing -> return gameGrid
        Just gg -> generateMap (updateSetList s1 s2 ss) gg


randomMap :: IO GameGrid
-- constructs a randomly-generated map out of the allRooms
randomMap = do
  let emptyRooms = L.map zeroExits allRooms
  g <- exampleGrid emptyRooms
  let initialSets = L.map mkSet (M.keys g)
  generateMap initialSets g


gridToMap :: GameGrid -> GameMap
-- pulls the map information out of a game grid
gridToMap g = mkMap
              . L.map (\tuple -> snd tuple)
              . M.toList $ g


instance Example GameState where
  example = do
    g <- randomMap
    return GameState {
              message = Nothing
              , grid = g
              , gmap = gridToMap g
              , universe = univ
              , player = you
            }