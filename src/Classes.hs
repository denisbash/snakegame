{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Classes(
    GameClass(..),
    GamePlayClass(..),
    gameLoop,
    gameLoopGen
) where

class GameClass s a | s -> a where -- without the func dep in gameLoop a is ambiguos; Besides gamestep is itself ambiguos
    evolve :: s -> s
    evolveWithInput :: (Maybe a, s) -> s
    isOver :: s -> Bool

class (GameClass s a, Monad m) => GamePlayClass s a m where
    gameStep :: s -> m (Maybe a, s)

gameLoop :: GamePlayClass s a m => s -> m (Maybe a, s)
gameLoop s0 = if isOver s0 then gameStep s0 else gameStep s0 >>= gameLoop . evolveWithInput 

gameLoopGen :: GamePlayClass s a m => (m (Maybe a, s) -> m (Maybe a, s)) -> s -> m (Maybe a, s)
gameLoopGen func s0 = if isOver s0 then gameStep s0 else do
    x <- gameStep s0
    func $ gameLoopGen func (evolveWithInput x)
