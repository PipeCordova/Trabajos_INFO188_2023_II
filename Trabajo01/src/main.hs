import System.Environment
import System.Random

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2
        then putStrLn "Uso: ./max <n> <s>" 
        else do
            let n = read (args !! 0) :: Int
                s = read (args !! 1) :: Int
                randomNumbers = take n (randoms (mkStdGen s) :: [Int])
            putStrLn "Números pseudoaleatorios generados:"
            print randomNumbers
