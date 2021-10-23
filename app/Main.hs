{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GamePlays ( drawCharBndry, prepareScreen, directionToChar, gameCleanUp )
import GameLogic (initialGame, Field(..), Snake(..), Point(..), Game (..))
import Auxiliary (ReaderT(..), WriterT (..), StateT(..), local, tailSafe, liftWriterTFunc)
import Classes (gameLoop, gameLoopGen)
import Control.Concurrent
import Control.Applicative
import System.Console.ANSI (setCursorPosition)
import System.Random
import System.IO (print)
import Data.Functor.Identity (Identity(runIdentity))



main :: IO ()
main = mainWTest --mainWTest --mainWTest' --mainWBot 

mainIO = do
    prepareScreen 
    g0 <- newStdGen 
    let game = initialGame g0   
    drawCharBndry '#' $ field game     
    gameLoop game
    putStrLn "That's it, folks!"    

testInput = " jhk l  k h  j"

mainTest :: IO()
mainTest = do
    g0 <- newStdGen 
    let res = runReaderT (gameLoopGen (local tailSafe) (initialGame g0)) testInput 
    case res of 
        Just (mDir, g) -> case mDir of
            Just dir -> putChar $ directionToChar dir
            Nothing -> print $ snake g--putStrLn "Got Maybe Char as Nothing in Just (Maybe Char, Game)"
        Nothing -> putStrLn "Got Nothing in Maybe (Maybe Char, Game)"
    putStrLn "Test run is completed"

mainWTest :: IO()
mainWTest = do
    g0 <- newStdGen 
    let res = runReaderT (runWriterT $ gameLoopGen (liftWriterTFunc $ local tailSafe) (initialGame g0)) testInput  
    case res of 
        Nothing -> putStrLn ":("
        Just (_, x::[Snake]) -> print x     
    putStrLn "WTest run is completed"

mainWTest' :: IO()
mainWTest' = do
    g0 <- newStdGen 
    let res = runStateT (runWriterT $ gameLoop (initialGame g0)) testInput  
    case res of 
        Nothing -> putStrLn ":("
        Just ((_,x :: [Snake]), _) -> print x     
    putStrLn "WTest run is completed"

mainWBot :: IO()
mainWBot = do
    g0 <- newStdGen 
    let (_, x::[Game]) = runIdentity $ runWriterT (gameLoop (initialGame g0)) 
    mapM_ (\g -> print (snake g, apple g)) x 
    putStrLn "Do you want to see replay? If YES, press y. Otherwise press any other key." 
    ch <- getChar
    if ch == 'y' then prepareScreen >> drawCharBndry '#' (field (head x)) >> mapM_ gameCleanUp x else putStrLn "OK"
    putStrLn "WBot run is completed"