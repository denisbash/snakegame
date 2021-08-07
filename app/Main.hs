module Main where

import Lib 
import Control.Concurrent 

main :: IO ()
main = do     
    prepareScreen
    -- ch <- liftChar 'x'
    -- putStrLn $ "worked; got " ++ [ch]
    -- putCharAtPosition '@' (20, 20)
    -- tid1 <- forkIO $ testChar 'q'    
    -- tid2 <- forkIO $ testChar 'j'
    -- tid3 <- forkIO $ testChar 'k'
    -- tid4 <- forkIO $ testChar 'l'    
    drawCharBndry '#' $ Field (5,5) (50,90) 
    gameLoop (liftChar 'q') $ Field (5,5) (50,90)    
    -- putStr $ show [tid1, tid2, tid3, tid4]
    

liftChar :: Char -> IO Char 
liftChar = pure 

testChar :: Char -> IO()
testChar ch = threadDelay 1000000 >> putChar ch