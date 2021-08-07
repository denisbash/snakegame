module Main where

import Lib 
import Control.Concurrent 

main :: IO ()
main = do     
    prepareScreen
    putCharAtPosition '@' (20, 20)
    tid <- forkIO testThread
    let f = Field (5,5) (50,90) in
        drawCharBndry '#' f >> gameLoop f    
    putStr $ show tid



testThread :: IO()
testThread = threadDelay 5000000 >> putChar 'l' >> threadDelay 1000000 >> putChar 'j'