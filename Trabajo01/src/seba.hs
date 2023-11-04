module Main where

import System.Environment
import System.Random
import Data.List.Split (chunksOf)

data Celda = Caminable | Lava | Obstaculo | Tesoro | Personaje deriving (Eq)

instance Show Celda where
  show Caminable = " "
  show Lava = "$"
  show Obstaculo = "L"
  show Tesoro = "X"
  show Personaje = "@"

data Game = Game
  { tamMapa :: Int,
    posTesoro :: (Int, Int),
    mapa :: [[Celda]]
  }

main :: IO ()
main = do
  args <- getArgs
  let n = read (args !! 0) :: Int
      s = read (args !! 1) :: Int

  let game = generateGame n s

  printGame game

generateGame :: Int -> Int -> Game
generateGame n s =
  let gen = mkStdGen s
      (x, gen') = randomR (0, n - 1) gen
      (y, _) = randomR (0, n - 1) gen'
      positions = [(i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1]]
      celdaList = map (\pos -> getCelda (mkStdGen (s * (posToSeed pos))) pos) positions
      celdas = chunksOf n celdaList
   in Game {tamMapa = n, posTesoro = (x, y), mapa = celdas}

getCelda :: StdGen -> (Int, Int) -> Celda
getCelda gen pos =
    let (r, _) = randomR (0 :: Int, 30) gen
        in if r == 0
            then Obstaculo
            else if r == 1
                then Lava
                else Caminable

-- Función para convertir una posición a una semilla para generar números aleatorios
posToSeed :: (Int, Int) -> Int
posToSeed (x, y) = mod (2654435761 * x + 2654435769 * y) 1000000007

printGame :: Game -> IO ()
printGame game = do
  putStrLn $ "Tamaño del mundo: " ++ show (tamMapa game)
  putStrLn $ "Tesoro en la posición: " ++ show (posTesoro game)
  putStrLn "Mapa:"
  mapM_ (putStrLn . concatMap show) (mapa game)


