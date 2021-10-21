{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module GamePlays
    ( drawCharBndry,
      prepareScreen,
      evWithInput,
      directionToChar
    ) where
import System.Console.ANSI
import System.IO
import GHC.Conc (threadDelay)
import Text.Read.Lex (isSymbolChar)
import GHC.Show (Show)

import GHC.Base (Alternative(empty))
import Data.List
import Data.Functor ((<&>))
import GameLogic (Game(..), Field(..), Snake(..), Direction(..), Point(..), Status(..),
    gameSpeed, evolveGame, changeSnakeDirection, isGameOver, initialGame, changeGameWithParams, setDirectionToApple)
import Classes (GameClass (..), GamePlayClass (gameStep))
import Auxiliary (ReaderT(..), local, WriterT, tell, liftWriterT)
import System.Random (newStdGen)
import Data.Functor.Identity (Identity)



--------------------------------- Instances -------------------------------------------------

type SnakeAuto = ReaderT [Char] Maybe

type SnakeWAuto = WriterT [Snake] SnakeAuto

type BotPlay = WriterT [(Snake, Point)] Identity

instance GameClass Game Direction where
    evolve = evolveGame
    evolveWithInput = evWithInput
    isOver = isGameOver

instance GamePlayClass Game Direction IO where
    gameStep = runOneStep

instance GamePlayClass Game Direction SnakeAuto where
    gameStep = runOneStepAuto

instance GamePlayClass Game Direction SnakeWAuto where
    gameStep = runOneStepWAuto

instance GamePlayClass Game Direction BotPlay where
    gameStep = runOneStepBot


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
runOneStepBot g = tell [(snake g, apple g)] >> (setDirectionToApple g, ) <$> handleGameIfOverAuto g

---------------------------- Auto GamePlay -------------------------------

runOneStepAuto :: Game -> SnakeAuto (Maybe Direction, Game)
runOneStepAuto g = local tail $ (,)<$>(transformInputToGameParams<$> whenKeyIsPressedAuto)<*> handleGameIfOverAuto g

handleGameIfOverAuto :: Monad m => Game -> m Game
handleGameIfOverAuto g = if isGameOver g then return g{status=OVER} else return g

whenKeyIsPressedAuto :: SnakeAuto (Maybe Char)
whenKeyIsPressedAuto = ReaderT $ Just . Just . head

--------------------------- WriterT Auto Gameplay ------------------------

runOneStepWAuto :: Game -> SnakeWAuto (Maybe Direction, Game)
runOneStepWAuto g = tell [snake g] >> liftWriterT (runOneStepAuto g)

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
    (,)<$>(transformInputToGameParams<$> whenKeyIsPressed)<*> handleGameIfOver g


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

whenKeyIsPressed :: IO (Maybe Char)
whenKeyIsPressed = hReady stdin >>= go
   where go True = getChar <&> Just
         go _    = return Nothing

