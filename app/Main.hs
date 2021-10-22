{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GamePlays ( drawCharBndry, prepareScreen, directionToChar, gameCleanUp )
import GameLogic (initialGame, Field(..), Snake(..), Point(..), Game (..))
import Auxiliary (ReaderT(..), WriterT (..))
import Classes (gameLoop)
import Control.Concurrent
import Control.Applicative
import System.IO
import System.Console.ANSI (setCursorPosition)
import System.Random
import System.IO (print)
import Data.Functor.Identity (Identity(runIdentity))

main :: IO ()
main = mainWBot 

mainIO = do
    prepareScreen 
    g0 <- newStdGen 
    let game = initialGame g0   
    drawCharBndry '#' $ field game     
    gameLoop game
    putStrLn "That's it, folks!"    

autoInput = "hk"--"  h   k l         j"

mainAuto :: IO()
mainAuto = do
    g0 <- newStdGen 
    let res = runReaderT (gameLoop (initialGame g0)) autoInput 
    case res of 
        Just (mDir,_) -> case mDir of
            Just dir -> putChar $ directionToChar dir
            Nothing -> putStrLn "Got Maybe Char as Nothing in Just (Maybe Char, Game)"
        Nothing -> putStrLn "Got Nothing in Maybe (Maybe Char, Game)"      
    
    putStrLn "Auto run is completed"

mainWAuto :: IO()
mainWAuto = do
    g0 <- newStdGen 
    let res = runReaderT (runWriterT (gameLoop (initialGame g0))) autoInput  
    case res of 
        Nothing -> putStrLn ":("
        Just (_, x::[Snake]) -> print x     
    putStrLn "WAuto run is completed"

mainWBot :: IO()
mainWBot = do
    g0 <- newStdGen 
    let (_, x::[Game]) = runIdentity $ runWriterT (gameLoop (initialGame g0)) 
    mapM_ (\g -> print (snake g, apple g)) x 
    putStrLn "Do you want to see replay? If YES, press y. Otherwise press any other key." 
    ch <- getChar
    if ch == 'y' then prepareScreen >> drawCharBndry '#' (field (head x)) >> mapM_ gameCleanUp x else putStrLn "OK"
    putStrLn "WBot run is completed"