{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module GamePlays
    ( drawCharBndry,
      prepareScreen,
      evWithInput,
      directionToChar,
      gameCleanUp
    ) where
import System.Console.ANSI ( clearScreen, setCursorPosition )
import System.IO
    ( hSetBuffering,
      hSetEcho,
      stdin,
      stdout,
      hReady,
      BufferMode(NoBuffering) )
import GHC.Conc (threadDelay)
import Text.Read.Lex (isSymbolChar)
import GHC.Show (Show)

import GHC.Base (Alternative(empty))
import Data.List
import Data.Functor ((<&>))
import GameLogic (Game(..), Field(..), Snake(..), Direction(..), Point(..), Status(..),
    gameSpeed, evolveGame, changeSnakeDirection, isGameOver, initialGame, changeGameWithParams, setDirectionToApple)
import Classes (GameClass (..), GamePlayClass (gameStep))
import Auxiliary (ReaderT(..), local, WriterT, tell, liftWriterT, StateT, getHead, pop)
import System.Random (newStdGen)
import Data.Functor.Identity (Identity)



--------------------------------- Instances -------------------------------------------------

type SnakeTest = ReaderT [Char] Maybe

type SnakeWTest = WriterT [Snake] SnakeTest

type BotPlay = WriterT [Game] Identity

type SnakeTest' = StateT [Char] Maybe

type SnakeWTest' = WriterT [Snake] SnakeTest'

instance GameClass Game Direction where
    evolve = evolveGame
    evolveWithInput = evWithInput
    isOver = isGameOver

instance GamePlayClass Game Direction IO where
    gameStep = runOneStep

instance GamePlayClass Game Direction SnakeTest where
    gameStep = runOneStepTest

instance GamePlayClass Game Direction SnakeWTest where
    gameStep = runOneStepWTest

instance GamePlayClass Game Direction BotPlay where
    gameStep = runOneStepBot

instance GamePlayClass Game Direction SnakeTest' where
    gameStep = runOneStepTest'

instance GamePlayClass Game Direction SnakeWTest' where
    gameStep = runOneStepWTest'
--------------------------------- General Stuff ---------------------------------------------

transformInputToGameParams :: Maybe Char -> Maybe Direction
transformInputToGameParams ch = case ch of
    Just 'k' -> Just UP
    Just 'j' -> Just DOWN
    Just 'h' -> Just LEFT
    Just 'l' -> Just RIGHT
    _        -> Nothing

directionToChar :: Direction -> Char
directionToChar UP    = 'k'
directionToChar DOWN  = 'j'
directionToChar LEFT  = 'h'
directionToChar RIGHT = 'l'

---------------------------- Bot GamePlay --------------------------------

runOneStepBot :: Game -> BotPlay (Maybe Direction, Game)
runOneStepBot g = tell [g] >> (setDirectionToApple g, ) <$> handleGameIfOverTest g

---------------------------- Auto Test GamePlay -------------------------------

runOneStepTest :: Game -> SnakeTest (Maybe Direction, Game)
runOneStepTest g = (,)<$>(transformInputToGameParams<$> getInputCharTest)<*> handleGameIfOverTest g

handleGameIfOverTest :: Monad m => Game -> m Game
handleGameIfOverTest g = if isGameOver g then return g{status=OVER} else return g

getInputCharTest :: SnakeTest (Maybe Char)
getInputCharTest = ReaderT $ Just . Just . head

--------------------------- Auto Test' GamePlay -------------------------------

runOneStepTest' :: Game -> SnakeTest' (Maybe Direction, Game)
runOneStepTest' g = (,)<$>(transformInputToGameParams<$> getInputCharTest' )<*> handleGameIfOverTest g

getInputCharTest' :: SnakeTest' (Maybe Char)
getInputCharTest' = pop >> getHead 

--------------------------- WriterT Auto Test Gameplay ------------------------

runOneStepWTest :: Game -> SnakeWTest (Maybe Direction, Game)
runOneStepWTest g = tell [snake g] >> liftWriterT (runOneStepTest g)

--------------------------- WriterT Auto Test Gameplay ------------------------

runOneStepWTest' :: Game -> SnakeWTest' (Maybe Direction, Game)
runOneStepWTest' g = tell [snake g] >> liftWriterT (runOneStepTest' g)

---------------------------- IO GamePlay ---------------------------------
clearLineCodeStr = "\ESC[0J"

putCharAtPosition :: Char -> (Int, Int) -> IO()
putCharAtPosition ch coors = do
    let (y, x) = coors in
     setCursorPosition y x
    putChar ch

drawCharBndry :: Char -> Field -> IO()
drawCharBndry char (Field (y0, x0) (y1, x1)) = do
    clearScreen
    mapM_ (putCharAtPosition char)  [(y0,x')| x' <- [x0..x1]]
    mapM_ (putCharAtPosition char)  [(y',x0)| y' <- [y0..y1]]
    mapM_ (putCharAtPosition char)  [(y1,x')| x' <- [x0..x1]]
    mapM_ (putCharAtPosition char)  [(y',x1)| y' <- [y0..y1]]

prepareScreen :: IO()
prepareScreen = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen
    setCursorPosition 0 0

showMessage :: Field -> String -> IO()
showMessage f s =   let (y, _) = lowerRight f
                        (_, x) = upperLeft f
                    in  setCursorPosition (y+2) x >> putStr s

gameCleanUp :: Game -> IO()
gameCleanUp g = do    
    drawGameCharPoints g
    threadDelay $ 10000 * gameSpeed
    eraseGameCharPoints g

runOneStep :: Game -> IO(Maybe Direction, Game)
runOneStep g = do
    gameCleanUp g
    (,)<$>(transformInputToGameParams<$> getInputCharIO)<*> handleGameIfOver g


evWithInput :: (Maybe Direction, Game) -> Game
evWithInput (res, g) = evolveGame $ changeGameWithParams res g

drawPoint :: Char -> Point -> IO()
drawPoint ch (y, x) = setCursorPosition y x >> putChar ch >> setCursorPosition y x

drawFigureWithChar :: [(Int, Int)] -> Char -> IO()
drawFigureWithChar xs ch = mapM_ (drawPoint ch) xs

clearField :: Field -> IO()
clearField = drawCharBndry '#'

handleGameIfOver :: Game -> IO Game
handleGameIfOver g = if isGameOver g
                        then showMessage (field g) ("GAME OVER! Your score is: " ++ show (score g) ++ ". Restart? (y/n)")
                            >> threadDelay 1000000 >> getChar >>= (\ch -> if ch == 'y' then showMessage (field g) clearLineCodeStr
                                                                    >> initialGame <$> newStdGen
                                                    else setCursorPosition 0 0 >> return g{status=OVER})
                    else return g


gameToCharPoints :: Game -> [(Char, Point)]
gameToCharPoints g  = ('@', apple g) : fmap ('o',) (positions . snake $ g)

drawCharPoint :: (Char, Point) -> IO()
drawCharPoint (ch, p) = drawPoint ch p

drawGameCharPoints :: Game -> IO()
drawGameCharPoints g = mapM_ drawCharPoint (gameToCharPoints g) >> setCursorPosition 0 0

eraseGameCharPoints :: Game -> IO()
eraseGameCharPoints g = mapM_ (\(ch, p) -> drawPoint ' ' p) (gameToCharPoints g) >> setCursorPosition 0 0

getInputCharIO :: IO (Maybe Char)
getInputCharIO = hReady stdin >>= go
   where go True = getChar <&> Just
         go _    = return Nothing

