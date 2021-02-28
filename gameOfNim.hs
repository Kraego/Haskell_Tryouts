import Control.Monad.IO.Class

printBoard :: [Int] -> Int -> IO()
printBoard [] _ = putStrLn ""
printBoard (x:xs) idx = do
    putStr (show idx)
    putStr ". "
    putStrLn (intToStars x)
    printBoard xs (idx+1)

play :: [Int] -> IO()
play board = do
    putStrLn "Enter row"
    input <- getLine
    let row = (read input :: Int)
    putStrLn "Enter count of removing"
    input <- getLine
    let count = (read input :: Int)

    if row < 1 || row > 5 then do
        putStrLn "Wrong row given (allowed 1-5)"
        play board
    else do
        if not (checkRemoval board row count) then do
            putStrLn "Cant remove that much!"
            printBoard board 1
            putStrLn "Retry"
            play board
        else do  
            let boardNow = subtractFromBoard board row count
            if boardNow == [0, 0, 0, 0, 0] then do
                putStrLn "U won!"
            else do
                printBoard boardNow 1
                play boardNow

main :: IO ()
main = do
    let board = [5,4,3,2,1]
    printBoard board 1
    play board

-- pure functions
intToStars :: Int -> String 
intToStars 0 = ""
intToStars x = "*" ++ intToStars (x-1)


-- row - 1 cause not using of by zero for non technicans
subtractFromBoard :: [Int] -> Int -> Int -> [Int]
subtractFromBoard board row count = replaceNth (row-1) ((board !! (row-1)) - count) board

checkRemoval :: [Int] -> Int -> Int -> Bool 
checkRemoval board row count = board !! (row - 1) >= count

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs