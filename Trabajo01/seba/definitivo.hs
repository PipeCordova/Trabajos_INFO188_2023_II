module Main where

import System.Environment
import Funciones

main :: IO ()
main = do
    args <- getArgs
    let n = read (args !! 0) :: Int
        s = read (args !! 1) :: Int

    let game = generateGame n s

    gameLoop game