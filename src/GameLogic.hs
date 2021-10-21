module GameLogic(
    Game(..),
    Field(..),
    Snake(..),
    Direction(..),
    Point(..),
    Status (..),
    gameSpeed,
    evolveGame,
    changeSnakeDirection,
    isGameOver,
    initialGame,
    changeGameWithParams,
    setDirectionToApple    
) where

import System.Random (Random(randomR), StdGen )

data Field = Field {
    upperLeft :: (Int, Int),
    lowerRight :: (Int, Int)}

type Point = (Int, Int)

data Direction = UP | DOWN | LEFT | RIGHT deriving Show

data Snake = Snake {
    positions ::[Point],
    direction :: Direction} deriving Show

data Status = RUNNING | OVER

type Apple = Point

data Game = Game {
    status :: Status,
    snake :: Snake,
    apple :: Apple,
    score :: Int,
    field :: Field,    
    randG :: StdGen        
}

appleScore = 1 :: Int

gameSpeed = 20 :: Int

initialSnake = Snake [(20,20)] RIGHT
initialApple = (10,10)
initialGame = Game RUNNING initialSnake initialApple 0 (Field (5,5) (30, 60)) 

isInsideTheField :: Field -> Point -> Bool 
isInsideTheField (Field (a, b) (c, d)) (y, x) =  y > a && x > b && y < c && x < d 

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
eatApple (Game st s a sc f g) = if hasSnakeFoundApple s a 
    then newApple (Game st (growSnakeWithPoint a s) a (sc + appleScore) f g)
    else Game st s a sc f g

evolveGame g = eatApple $ g{snake=moveSnake . snake $ g}

getSnakeEnd :: Snake -> Point
getSnakeEnd  = last . positions 

newApple :: Game -> Game
newApple (Game st s ap sc (Field a b) r) = let 
     (y, r1) = randomR(fst a + 1, fst b - 1) r
     (x, r2) = randomR(snd a + 1, snd b - 1) r1
     in Game st s (y,x) sc (Field a b) r2

isGameOver :: Game -> Bool 
isGameOver g = doesSelfIntersect (snake g) || not (isInsideTheField (field g) (head . positions . snake $ g)) 

doPointsCoincide :: Point -> Point -> Bool 
doPointsCoincide p1 p2 = p1 == p2 

doesSelfIntersect :: Snake -> Bool 
doesSelfIntersect (Snake [] _) = False 
doesSelfIntersect (Snake [x] _) = False
doesSelfIntersect (Snake (x:xs) _) = or $ fmap (doPointsCoincide x) xs

changeGameWithParams :: Maybe Direction -> Game -> Game
changeGameWithParams Nothing  g = g
changeGameWithParams (Just d) g =  g {snake = changeSnakeDirection (snake g) d}

directionFromPointToPoint :: Point -> Point -> Maybe Direction
directionFromPointToPoint (y0, x0) (y, x)
    | y>y0 = Just DOWN
    | y<y0 = Just UP
    | x<x0 = Just LEFT
    | x>x0 = Just RIGHT
    | otherwise = Nothing 

setDirectionToApple :: Game -> Maybe Direction
setDirectionToApple g = directionFromPointToPoint (head (positions . snake $ g)) (apple g) 
