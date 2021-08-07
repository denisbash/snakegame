module Lib
    ( putCharAtPosition,
      drawCharBndry,
      prepareScreen,
      --gameLoop,
      Field(..),
      --gameLoopFake,
      gameLoop
    ) where
import System.Console.ANSI
import System.IO


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
    setCursorPosition ((y0+y1)`div` 2) ((x0+x1) `div` 2)

prepareScreen :: IO()
prepareScreen = do
    hSetBuffering stdin NoBuffering 
    --hSetEcho stdin False
    clearScreen
    setCursorPosition 0 0 

-- gameLoop :: Int -> Int -> IO()
-- gameLoop y x = do 
--     setCursorPosition y x   
--     ch <- getChar
--     case ch of
--             'j' -> gameLoop (y+1) x       
--             'k' -> gameLoop y (x+1)
--             _ ->  putStr "not a valid key" >> gameLoop y x
     
                
        
-- gameLoopFake :: IO()
-- gameLoopFake = do
--     ch <- getChar 
--     putChar ch
--     pos <- getCursorPosition
--     case pos of
--         Just (y, x) -> setCursorPosition (y+1) (x+1)
--         Nothing -> putStr "Nothing -- game's over"                        
--     case ch of
--         'q' -> putChar ch
--         _ -> gameLoopFake

data Field = Field {
    upperLeft :: (Int, Int),
    lowerRight :: (Int, Int)}


showMessage :: Field -> String -> IO()
showMessage f s = let (y, x) = lowerRight f in
    setCursorPosition (y+2) x >> putStr s


isInsideTheField :: Field -> Int -> Int -> Bool 
isInsideTheField (Field (a, b) (c, d)) y x =  y > a && x > b && y < c && x < d 

shiftCursor :: Char -> Field -> Int -> Int -> IO()
shiftCursor ch f y x = do
    if not (isInsideTheField f y x) 
    then showMessage f "Not inside the field" >> setCursorPosition y x
    else 
        case ch of
                'j' -> if isInsideTheField f (y+1) x 
                    then setCursorPosition (y+1) x
                    else showMessage f "cannot move down" >> setCursorPosition y x      
                'k' -> if isInsideTheField f (y-1) x
                    then setCursorPosition (y-1) x
                    else showMessage f "cannot move up" >> setCursorPosition y x 
                'h' -> if isInsideTheField f y (x-1) 
                    then setCursorPosition y (x-1)
                    else showMessage f "cannot move left" >> setCursorPosition y x 
                'l' -> if isInsideTheField f y (x+1) 
                    then setCursorPosition y (x+1)
                    else showMessage f "cannot move right" >> setCursorPosition y x 
                _ ->  showMessage f "not a valid key" >> setCursorPosition y x

    
gameLoop :: IO Char -> Field -> IO()
gameLoop ioChar f = do
    pos <- getCursorPosition
    ch <- ioChar 
    if ch == 'q' 
    then showMessage f "Game's over"
    else     
        case pos of
            Just (y, x) -> shiftCursor ch f y x >> gameLoop ioChar f
            Nothing -> showMessage f "Not a valid pos" >> setCursorPosition 1 1 >> gameLoop ioChar f