{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Classes(
    GameClass(..),
    GamePlayClass(..),
    gameLoop
) where
import Control.Applicative (Alternative)

class GameClass s a | s -> a where -- without the func dep in gameLoop a is ambiguos; Besides gamestep is itself ambiguos
    evolve :: s -> s
    evolveWithInput :: (Maybe a, s) -> s
-- Need Alternative to return empty game if game is over in gameStep
class (GameClass s a, Alternative m, Monad m) => GamePlayClass s a m where
    gameStep :: s -> m (Maybe a, s)


gameLoop :: GamePlayClass s a m => s -> m (Maybe a, s)
gameLoop s0 = gameStep s0 >>= gameLoop . evolveWithInput 
