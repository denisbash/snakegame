module Main where

import Lib
import Control.Concurrent
import Control.Applicative
import System.IO
import System.Console.ANSI (setCursorPosition)
import System.Random
main :: IO ()
main = do
    prepareScreen    
    drawCharBndry '#' $ Field (5,5) (50,90)    
    --handle <- openFile "/home/denis/Desktop/haskell/snake-game/testCursorPath.txt" ReadMode    
    --gameLoop (hGetChar handle) $ Field (5,5) (50,90)
    g0 <- newStdGen  
    gameLoop getChar (initialGame g0)
    


liftChar :: Char -> IO Char
liftChar ch = do
    threadDelay 1000000
    if ch == ' ' then empty
    else pure ch

liftCharsAlt :: Char -> Char -> IO Char 
liftCharsAlt x y = (threadDelay 1000000 >> pure x) <|> (threadDelay 1000000 >> pure y)

liftChars :: [Char] -> IO Char
liftChars xs = foldr ((>>) . liftChar) (liftChar 'q') xs

testChar :: Char -> IO()
testChar ch = threadDelay 1000000 >> putChar ch

randomBot :: Int -> IO()
randomBot i = do
     x <- randomRIO(1,10) 
     print $ x>i


