module Main where

import System.Environment
import System.Random
import Funciones

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2
        then putStrLn "Debe ejecutar el codigo de la siguente forma\n./max <n> <s>\nn = Tamaño del mapa\ns = Semilla"
        else do
            let n = read (args !! 0) :: Int
                s = read (args !! 1) :: Int
            if n < 6
                then putStrLn "El valor de n debe ser mayor o igual a 6 para el correcto funcionamiento."
                else do
                    let gen = mkStdGen s
                        (x, gen') = randomR (0, n - 1) gen
                        (y, gen'') = randomR (0, n - 1) gen'
                        (x',gen''') = checkPosition n (x, gen'')
                        (y',_) = checkPosition n (y, gen''')

                    let game = generateGame n s (x, y) (x',y')

                    gameLoop game