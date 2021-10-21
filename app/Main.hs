{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GamePlays ( drawCharBndry, prepareScreen, directionToChar )
import GameLogic (initialGame, Field(..), Snake(..), Point(..), Game (field))
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
    let (_, x::[(Snake, Point)]) = runIdentity $ runWriterT (gameLoop (initialGame g0)) 
    print x     
    putStrLn "WBot run is completed"