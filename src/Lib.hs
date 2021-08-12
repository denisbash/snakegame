module Lib
    ( drawCharBndry,
      prepareScreen,
      gameLoop,
      initialGame,
      Field(..),
      Game(..),                
    ) where
import System.Console.ANSI
import System.IO
import GHC.Conc (threadDelay)
import Text.Read.Lex (isSymbolChar)
import GHC.Show (Show)
import System.Random
import GHC.Base (Alternative(empty))
import Data.List

data Field = Field {
    upperLeft :: (Int, Int),
    lowerRight :: (Int, Int)}

-- showInfo :: Game -> IO()
-- showInfo g = setCursorPosition (fst (lowerRight . field g))  (fst (upperLeft . field g)) >> print snake g >> print pointsToErase g 

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
appleScore = 1 :: Int

gameSpeed = 3 :: Int

initialSnake = Snake [(20,20)] RIGHT
initialApple = (10,10)
initialGame = Game initialSnake initialApple 0 (Field (5,5) (40, 90))  

putCharAtPosition :: Char -> (Int, Int) -> IO()

putCharAtPosition ch coors = do 
    let (y, x) = coors in     
     setCursorPosition y x
    --threadDelay 10000
    putChar ch

drawCharBndry :: Char -> Field -> IO()
drawCharBndry char (Field (y0, x0) (y1, x1)) = do
    clearScreen            
    mapM_ (putCharAtPosition char)  [(y0,x')| x' <- [x0..x1]]
    mapM_ (putCharAtPosition char)  [(y',x0)| y' <- [y0..y1]]
    mapM_ (putCharAtPosition char)  [(y1,x')| x' <- [x0..x1]]
    mapM_ (putCharAtPosition char)  [(y',x1)| y' <- [y0..y1]]
    --setCursorPosition ((y0+y1)`div` 2) ((x0+x1) `div` 2)

prepareScreen :: IO()
prepareScreen = do
    hSetBuffering stdin NoBuffering 
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen
    setCursorPosition 0 0 

showMessage :: Field -> String -> IO()
showMessage f s = let (y, x) = lowerRight f in
    setCursorPosition (y+2) x >> putStr s


isInsideTheField :: Field -> Point -> Bool 
isInsideTheField (Field (a, b) (c, d)) (y, x) =  y > a && x > b && y < c && x < d 

gameLoop :: IO Char -> Game -> IO()
gameLoop iochar g = do    
    drawGameCharPoints g        
    handleGameOver . isGameOver $ g 
    threadDelay $ 100000 * gameSpeed
    res <- whenKeyIsPressed stdin getChar    
    eraseGameCharPoints g 
    case res of
        Nothing -> gameLoop iochar (evolveGame g)
        Just ch -> gameLoop iochar (evolveGame $ (changeGameWithParams . transformInputToGameParams $ ch) g) 
    

transformInputToGameParams :: Char -> Maybe Direction
transformInputToGameParams ch = case ch of 
    'k' -> Just UP
    'j' -> Just DOWN
    'h' -> Just LEFT
    'l' -> Just RIGHT 
    _ -> Nothing 

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
    then newApple (Game (growSnakeWithPoint a s) a (sc+appleScore) f g)
    else Game s a sc f g

evolveGame g = eatApple $ g{snake=moveSnake . snake $ g}

eraseApple a = case a of
    Nothing -> []
    Just x -> [x] 

getSnakeEnd :: Snake -> Point
getSnakeEnd  = last . positions 

drawSnake :: Snake -> IO()
drawSnake (Snake ps _) = drawFigureWithChar ps 'o'

drawApple :: Apple -> IO()
drawApple = drawPoint '@' 


drawGame :: Game -> IO()
drawGame  g = drawSnake  (snake g) >> drawApple  (apple g) >> setCursorPosition 0 0

drawFigureWithChar :: [(Int, Int)] -> Char -> IO()
drawFigureWithChar xs ch = mapM_ (drawPoint ch) xs
    
clearField :: Field -> IO()
clearField = drawCharBndry '#' 

newApple :: Game -> Game
newApple (Game s ap sc (Field a b) r) = let 
     (y, r1) = randomR(fst a, fst b) r
     (x, r2) = randomR(snd a, snd b) r1
     in Game s (y,x) sc (Field a b) r2

isGameOver :: Game -> Bool 
isGameOver g = not $ isInsideTheField (field g) (head . positions . snake $ g)

handleGameOver :: Bool -> IO()
handleGameOver True = empty
handleGameOver False = return()

gameToCharPoints :: Game -> [(Char, Point)]
gameToCharPoints g  = ('@', apple g) : fmap (\p -> ('o', p)) (positions . snake $ g)

drawCharPoint :: (Char, Point) -> IO()
drawCharPoint (ch, p) = drawPoint ch p

drawGameCharPoints :: Game -> IO()
drawGameCharPoints g = mapM_ drawCharPoint (gameToCharPoints g) >> setCursorPosition 0 0

eraseGameCharPoints :: Game -> IO()
eraseGameCharPoints g = mapM_ (\(ch, p) -> drawPoint ' ' p) (gameToCharPoints g) >> setCursorPosition 0 0

whenKeyIsPressed :: Handle -> IO a -> IO (Maybe a)
whenKeyIsPressed handle getCh = hReady handle >>= go
   where go True = getCh >>= return . Just
         go _    = return Nothing


