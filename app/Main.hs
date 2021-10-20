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
    drawCharBndry '#' $ Field (5,5) (30,60)    
    --handle <- openFile "/home/denis/Desktop/haskell/snake-game/testCursorPath.txt" ReadMode    
    --gameLoop (hGetChar handle) $ Field (5,5) (50,90)
    g0 <- newStdGen  
    gameLoop stdin getChar (initialGame g0)    


