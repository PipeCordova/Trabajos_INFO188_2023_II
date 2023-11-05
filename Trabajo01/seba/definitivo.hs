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

gameLoop :: Game -> IO ()
gameLoop game
    | posPersonaje game == posTesoro game = do
      printGame game
      putStrLn "¡Has encontrado el tesoro! ¡Has ganado!"
    | elem (posPersonaje game) (posLava game) = do
        printGame game
        putStrLn "¡Caíste a la lava!¡GAME OVER!"
        return () 
    | otherwise = do
      printGame game
      putStrLn "Ingrese una opción (W/A/S/D para moverse, Q para salir): "
      option <- getLine
      let game' = case option of
            "W" -> arriba game
            "w" -> arriba game
            "A" -> izquierda game
            "a" -> izquierda game
            "S" -> abajo game
            "s" -> abajo game
            "D" -> derecha game
            "d" -> derecha game
            "Q" -> game
            _   -> game
      if option /= "Q"
            then gameLoop game'
            else return ()

-- Función simple que mueve el personaje hacia arriba, verificando que sea un movimiento válido y que no se salga de los bordes
arriba :: Game -> Game
arriba game@Game{posPersonaje=(x,y), mapa=mapa} = 
    let newPos = (x, y-1)
    in if y > 0 && checkMov newPos mapa
        then actualizarMapa game newPos
        else game

-- Función simple que mueve el personaje hacia la izquierda, verificando que sea un movimiento válido y que no se salga de los bordes
izquierda :: Game -> Game
izquierda game@Game{posPersonaje=(x,y), mapa=mapa} = 
    let newPos = (x-1, y)
    in if x > 0 && checkMov newPos mapa
        then actualizarMapa game newPos
        else game

-- Función simple que mueve el personaje hacia abajo, verificando que sea un movimiento válido y que no se salga de los bordes
abajo :: Game -> Game
abajo game@Game{posPersonaje=(x,y), tamMapa=n, mapa=mapa} = 
    let newPos = (x, y+1)
    in if y < n-1 && checkMov newPos mapa
        then actualizarMapa game newPos
        else game

-- Función simple que mueve el personaje hacia la derecha, verificando que sea un movimiento válido y que no se salga de los bordes
derecha :: Game -> Game
derecha game@Game{posPersonaje=(x,y), tamMapa=n, mapa=mapa} = 
    let newPos = (x+1, y)
    in if x < n-1 && checkMov newPos mapa
        then actualizarMapa game newPos
        else game