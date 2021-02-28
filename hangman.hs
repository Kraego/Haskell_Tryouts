import Control.Monad.IO.Class
import System.IO

-- Toplevel
main :: IO ()
main = do
    putStrLn "Think of a word: "
    word <- sgetLine
    putStrLn "Try to guess it:"
    play word

-- Play action
play :: String -> IO ()
play word = do
    putStr "? "
    guess <- getLine
    if guess == word then
        putStrLn "You got it!"
    else do
        putStrLn (match word guess)
        play word

-- Helperfunctions
sgetLine :: IO String -- secret get line
sgetLine = do
    x <- getCh
    if x == '\n' then do
        putChar x
        return []
    else do
        putChar '*'
        xs <- sgetLine
        return (x:xs)

getCh :: IO Char -- get character without echo
getCh = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

-- pure function!!
match :: String -> String -> String
match xs ys = map (\x -> if elem x ys then x else '*') xs