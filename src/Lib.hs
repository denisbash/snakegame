{-# LANGUAGE TupleSections #-}
module Lib
    ( drawCharBndry,
      prepareScreen,
      gameLoop,
      initialGame,
      Field(..),
      Game(..), 
      evolveGame,
      evWithInput             
    ) where
import System.Console.ANSI
import System.IO
import GHC.Conc (threadDelay)
import Text.Read.Lex (isSymbolChar)
import GHC.Show (Show)
import System.Random
import GHC.Base (Alternative(empty))
import Data.List
import Data.Functor ((<&>))
import Classes (GameClass)


data Field = Field {
    upperLeft :: (Int, Int),
    lowerRight :: (Int, Int)}

type Point = (Int, Int)

data Direction = UP | DOWN | LEFT | RIGHT deriving Show

data Snake = Snake {
    positions ::[Point],
    direction :: Direction} deriving Show

type Apple = Point

data Game = Game {
    snake :: Snake,
    apple :: Apple,
    score :: Int,
    field :: Field,    
    randG :: StdGen     
}

clearLineCodeStr = "\ESC[0J"
appleScore = 1 :: Int

gameSpeed = 20 :: Int

initialSnake = Snake [(20,20)] RIGHT
initialApple = (10,10)
initialGame = Game initialSnake initialApple 0 (Field (5,5) (30, 60))  

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

isInsideTheField :: Field -> Point -> Bool 
isInsideTheField (Field (a, b) (c, d)) (y, x) =  y > a && x > b && y < c && x < d 

gameLoop :: Game -> IO (Maybe Char, Game)
gameLoop g = do    
    drawGameCharPoints g 
    threadDelay $ 10000 * gameSpeed 
    eraseGameCharPoints g      
    g' <- handleGameIfOver g     
    res <- whenKeyIsPressed     
    gameLoop $ evolveGame $ (changeGameWithParams . transformInputToGameParams $ res) g' 

gameCleanUp :: Game -> IO()
gameCleanUp g = do
    drawGameCharPoints g 
    threadDelay $ 10000 * gameSpeed 
    eraseGameCharPoints g 

runOneStep :: Game -> IO(Maybe Char, Game)
runOneStep g = do
    gameCleanUp g
    (,)<$>whenKeyIsPressed <*> handleGameIfOver g     

evWithInput :: (Maybe Char, Game) -> Game
evWithInput (res, g) = evolveGame $ (changeGameWithParams . transformInputToGameParams $ res) g

transformInputToGameParams :: Maybe Char -> Maybe Direction
transformInputToGameParams ch = case ch of 
    Just 'k' -> Just UP
    Just 'j' -> Just DOWN
    Just 'h' -> Just LEFT
    Just 'l' -> Just RIGHT 
    _        -> Nothing 

changeGameWithParams :: Maybe Direction -> Game -> Game
changeGameWithParams Nothing  g = g
changeGameWithParams (Just d) g =  g {snake = changeSnakeDirection (snake g) d}

drawPoint :: Char -> Point -> IO()
drawPoint ch (y, x) = setCursorPosition y x >> putChar ch >> setCursorPosition y x 

changeSnakeDirection :: Snake -> Direction -> Snake
changeSnakeDirection (Snake ps _) = Snake ps 

movePointInDirection :: Point -> Direction -> Point
movePointInDirection (y,x) d = case d of
    UP -> (y-1,x)
    DOWN -> (y+1,x)
    LEFT -> (y, x-1)
    RIGHT -> (y, x+1)

moveSnakeTail :: Point -> [Point] -> [Point]
moveSnakeTail h ps =
    case ps of
        [] -> []
        x:xs -> h:moveSnakeTail x xs

moveSnake :: Snake -> Snake
moveSnake (Snake p d) = 
    case p of 
        [] -> Snake p d
        h:ts -> Snake (movePointInDirection h d:moveSnakeTail h ts) d

hasSnakeFoundApple :: Snake -> Apple -> Bool 
hasSnakeFoundApple (Snake [] _) a = False  
hasSnakeFoundApple (Snake (x:xs) d) y = nextPointInDirection x d==y

nextPointInDirection :: Point -> Direction -> Point
nextPointInDirection = movePointInDirection
    
growSnakeWithPoint :: Point -> Snake -> Snake
growSnakeWithPoint x (Snake ps d) = Snake (x:ps) d

eatApple :: Game -> Game
eatApple (Game s a sc f g) = if hasSnakeFoundApple s a 
    then newApple (Game (growSnakeWithPoint a s) a (sc + appleScore) f g)
    else Game s a sc f g

evolveGame g = eatApple $ g{snake=moveSnake . snake $ g}

getSnakeEnd :: Snake -> Point
getSnakeEnd  = last . positions 

drawFigureWithChar :: [(Int, Int)] -> Char -> IO()
drawFigureWithChar xs ch = mapM_ (drawPoint ch) xs
    
clearField :: Field -> IO()
clearField = drawCharBndry '#' 

newApple :: Game -> Game
newApple (Game s ap sc (Field a b) r) = let 
     (y, r1) = randomR(fst a + 1, fst b - 1) r
     (x, r2) = randomR(snd a + 1, snd b - 1) r1
     in Game s (y,x) sc (Field a b) r2

isGameOver :: Game -> Bool 
isGameOver g = doesSelfIntersect (snake g) || not (isInsideTheField (field g) (head . positions . snake $ g)) 

doPointsCoincide :: Point -> Point -> Bool 
doPointsCoincide p1 p2 = p1 == p2 

doesSelfIntersect :: Snake -> Bool 
doesSelfIntersect (Snake [] _) = False 
doesSelfIntersect (Snake [x] _) = False
doesSelfIntersect (Snake (x:xs) _) = or $ fmap (doPointsCoincide x) xs

handleGameIfOver :: Game -> IO Game
handleGameIfOver g = if isGameOver g 
                        then showMessage (field g) ("GAME OVER! Your score is: " ++ show (score g) ++ ". Restart? (y/n)")
                            >> threadDelay 1000000 >> getChar >>= (\ch -> if ch == 'y' then showMessage (field g) clearLineCodeStr 
                                                                    >> initialGame <$> newStdGen    
                                                    else setCursorPosition 0 0 >> empty)
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
   where go True = getChar >>= return . Just
         go _    = return Nothing


--instance GameClass

-- xs = "jklhkjlc"

-- f :: String -> [Char]
-- f ch = do
--     x <- xs
--     if x=='c' then "game over at: "++x:ch else "x"--f $ x:ch

