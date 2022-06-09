module Direction where

data Direction
    = N
    | S
    | E
    | W
    deriving Eq

instance Show Direction where
    show d = case d of
        N -> "north"
        S -> "south"
        W -> "west"
        E -> "east"

directions :: [Direction]
directions = [N, S, E, W]