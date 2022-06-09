module Main where

import Control.Monad.State

import GameIO
import GameState
import Example

main :: IO ()
main = example
       >>= evalStateT (opening >> forever repl)
