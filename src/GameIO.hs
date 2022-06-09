module GameIO where

import Control.Monad.State
import System.Exit (exitSuccess)
import System.IO
import Data.List (intercalate)

import GameState
import Player
import Room
import Command
import Item

-- 1.4.1 Exercise: GameIO datatype
type GameIO a = StateT GameState IO a


-- 1.4.2
effectChange :: (GameState -> GameState) -> GameIO ()
-- takes a transition between game states as an input, 
-- and outputs a value of type GameIO that performs that
-- transition within the monadic GameIO value
effectChange transition = get >>= put . transition

-- 1.4.3
prompt :: GameIO ()
-- prints the string "-> " (without a newline character)
-- to the console,
-- which functions for the user as a visible prompt:
prompt = lift $ putStr "->" >> hFlush stdout


-- 1.4.4
printMessage :: GameIO ()
-- checks the current state of the game,
--  and if there is Just value in the message field of the
-- current game state, it prints the message to the screen,
-- then sets the game state message field to Nothing.
-- If there is a Nothing value in the message field of
-- the current game state, it does nothing.
printMessage = do
    state <- get
    case message state of
        Just msg -> do
            lift $ putStrLn msg
            effectChange $ setMessage ""
        Nothing -> pure ()


-- 1.4.5
printDescription :: GameIO ()
--  prints a description of the room where the player is
-- in the current game state
printDescription =
  get >>= lift . putStrLn . desc . currentRoom


-- 1.4.6
printObjects :: GameIO ()
-- prints "You see the following objects:" 
-- followed by a list of all the items in the room
-- where the player is—assuming there are any. 
-- If there are no objects in the room where the player is,
-- it does nothing
printObjects = do
    state <- get
    if roomHasObjects state
    then do 
        lift $ putStrLn "You see the following objects:"
        lift
          . putStrLn
          . intercalate "\n"
          . map show
          . nearbyObjects $ state
    else pure ()


-- 2.0.1
printExits :: GameIO ()
-- prints "There are exits in the following directions:" 
-- followed by a list of all the directions there are exits
-- in, in the room where the player currently is.
-- If there are no exits in the room where the player
-- currently is, it does nothing:
printExits = do
    state <- get
    let exs = exits $ currentRoom state
    case exs of
        [] -> pure ()
        _ -> do 
            lift $ putStrLn "There are exits in the following directions:"
            lift
              . putStrLn
              . intercalate "\n"
              . map (show . fst) $ exs


-- 2.0.2
printInventory :: GameIO ()
-- checks the player's current inventory, and if it's empty, 
-- prints "You aren't carrying anything."
-- If the player's current inventory is nonempty, 
-- then it prints "You are carrying the following items:" 
-- followed by a list of all the ItemName-s in the player's inventory
printInventory = do
    state <- get
    case currentInventory state of
        [] -> lift . putStrLn $ "You aren't carrying anything."
        objs -> do 
            lift . putStrLn
              $ "You are carrying the following items:"
            lift
              . putStrLn
              . intercalate "\n"
              . map show $ objs


-- 2.0.3
actionOverList :: (ItemName -> GameState -> GameState)
               -> [ItemName]
               -> GameIO ()
-- takes a function describing an action on items (i.e. like taking or dropping them) and a list of item names as input, and performs the action on each item in the list, in order.
-- Each time it executes an action, it runs printMessage.
actionOverList action inames = do
    let
      actionAndPrint iname =
        effectChange (action iname)
        >> printDashLine
        >> printMessage
        >> printDashLine
    sequence_ $ fmap actionAndPrint inames


displayMap :: GameIO ()
-- print the map of the current game state, with a star
-- located wherever the player is.
displayMap = do
    state <- get
    let roomName = rname . currentRoom $ state
    lift . putStrLn
      $ showGrid (Just roomName) (grid state) 

-- 2.0.4
finishGame :: GameIO ()
-- printing a success message to the screen, then quits
-- the program. 
finishGame = printDashLine
             >> printMultipleLines winLines
             >> printDashLine
             >> lift exitSuccess


printMultipleLines :: [String] -> GameIO()
-- take a lists of lines and print them one by one
printMultipleLines = lift . sequence_ . fmap putStrLn


printDashLine :: GameIO()
-- print a blank line to screen
printDashLine = lift . putStrLn $ ""


winLines :: [String]
winLines = [
    "You have collected all three Dealthly Hallows."
    , "And you killed Lord Voldemort!"
    , "Congrats! You win!"
  ]


-- 2.0.5
exit :: GameIO ()
-- rints the message "Goodbye!", then exits the game with
-- a zero exit status:
exit = printDashLine
       >> printMultipleLines exitLines
       >> printDashLine
       >> lift exitSuccess


exitLines :: [String]
exitLines = [
    "Goodbye!"
  ]


-- 2.0.6
checkGameOver :: GameIO ()
-- checks whether the current game state is the winning state. 
checkGameOver = do
    state <- get
    if haveWonGame state
    then finishGame
    else pure ()


-- 2.0.7
syntaxError :: GameIO ()
-- prints the message 'I don't understand that'
syntaxError = printDashLine
              >> lift (putStrLn "I don't understand that.")
              >> printDashLine


-- 2.0.8
opening :: GameIO ()
-- prints the message "Welcome to Functional Adventure!"
opening = printDashLine
          >> printMultipleLines openingLines
          >> printDashLine


openingLines :: [String]
openingLines = [
    "Hurry! Collect all three Deathly Hallows!"
    , "Find them, before any Death Eaters do..."
  ]


-- 2.0.9
performCommand :: Command -> GameIO ()
-- takes any Command as an input,
-- and executes the action corresponding to the command. 
performCommand Inventory = printDashLine
                           >> printInventory
                           >> printDashLine
performCommand Look = printDashLine
                      >> printDescription
                      >> printDashLine
                      >> printObjects
                      >> printDashLine
                      >> printExits
                      >> printDashLine
performCommand (Take inames) = actionOverList takeItem inames
performCommand (Drop inames) = actionOverList dropItem inames
performCommand (Move dir) = effectChange (move dir)
                            >> printDashLine
                            >> printMessage
                            >> printDashLine
performCommand Map = displayMap
performCommand Exit = exit


-- 2.0.10
performConjunction :: Conjunction -> GameIO ()
-- performs every command in a Conjunction, in order
performConjunction cmdList =
  sequence_ $ fmap performCommand cmdList


-- 2.0.11
parseConjunction :: String -> GameIO ()
-- parses an input string, and if the parse succeeds, runs
-- performConjunction on the result.
-- If the parse fails, it runs syntaxError
parseConjunction str =
    case parseInput str of
        Just cmds -> performConjunction cmds
        Nothing -> syntaxError


-- 2.0.12
repl :: GameIO ()
-- performs one round of printing the prompt, getting input
-- from the user, parsing it, performing the command the
-- input denotes if the parse succeeds and printing a
-- syntax error message otherwise, then running checkGameOver
repl = prompt
       >> lift getLine
       >>= parseConjunction
       >> checkGameOver
