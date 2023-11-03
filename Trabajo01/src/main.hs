import System.Random
import Control.Monad (replicateM)
import System.Environment (getArgs)

data Cell = Walkable | Obstacle | Lava | Player | Treasure deriving (Eq)

instance Show Cell where
    show Walkable = " "
    show Obstacle = "L"
    show Lava     = "$"
    show Player   = "@"
    show Treasure = "X"

type Grid = [[Cell]]
type Position = (Int, Int)

main :: IO ()
main = do
    args <- getArgs
    let n = read (args !! 0) :: Int
        seed = read (args !! 1) :: Int
        gen = mkStdGen seed
    gameLoop gen n (1,1) (n-2,n-2)

gameLoop :: StdGen -> Int -> Position -> Position -> IO ()
gameLoop gen n playerPos treasurePos = do
    let (grid, newGen) = generateGrid gen n playerPos treasurePos
    printGrid grid
    putStrLn "Ingrese una acción de movimiento -> W,A,S,D, y R"
    action <- getLine
    case action of
        "R" -> main
        [move] -> if move `elem` "WASD" then
                    let newPos = movePlayer move playerPos grid
                    in if newPos == treasurePos then
                            putStrLn "¡Has encontrado el tesoro y ganado el juego!"
                        else
                            gameLoop newGen n newPos treasurePos
                else
                    gameLoop gen n playerPos treasurePos
        _ -> gameLoop gen n playerPos treasurePos

generateGrid :: StdGen -> Int -> Position -> Position -> (Grid, StdGen)
generateGrid gen n playerPos treasurePos =
    let (grid, newGen) = runRand (replicateM n (replicateM n randomCell)) gen
        gridWithPlayer = updateGrid playerPos Player grid
        gridWithTreasure = updateGrid treasurePos Treasure gridWithPlayer
    in (gridWithTreasure, newGen)

randomCell :: RandomGen g => g -> (Cell, g)
randomCell gen =
    let (value, newGen) = randomR (1, 100) gen
    in case value of
        _ | value <= 60 -> (Walkable, newGen)
        _ | value <= 80 -> (Obstacle, newGen)
        _               -> (Lava, newGen)

printGrid :: Grid -> IO ()
printGrid = mapM_ (putStrLn . concatMap show)

updateGrid :: Position -> Cell -> Grid -> Grid
updateGrid (x,y) cell grid = 
    take y grid ++
    [take x (grid !! y) ++ [cell] ++ drop (x+1) (grid !! y)] ++
    drop (y+1) grid

movePlayer :: Char -> Position -> Grid -> Position
movePlayer move (x,y) grid = 
    let newPos = case move of
                    'W' -> (x, y-1)
                    'A' -> (x-1, y)
                    'S' -> (x, y+1)
                    'D' -> (x+1, y)
                    _   -> (x, y)
    in if validMove newPos grid then newPos else (x,y)

validMove :: Position -> Grid -> Bool
validMove (x,y) grid = 
    not (x < 0 || y < 0 || x >= length (head grid) || y >= length grid || grid !! y !! x `elem` [Obstacle, Lava])

runRand :: RandomGen g => (g -> (a, g)) -> g -> (a, g)
runRand f gen = f gen