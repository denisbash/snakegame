{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Classes(
    GameClass(..),
    GamePlayClass(..)
) where

class GameClass s a g | g -> s a where -- without the func dep in gameLoop a is ambiguos; Besides gamestep is itself ambiguos
    evolve :: g -> s -> s
    evolveWithInput :: g -> (Maybe a, s) -> s

class (GameClass s a g, Monad m) => GamePlayClass s a g m p | p -> g where
    gameStep :: p -> s -> m (Maybe a, s)


gameLoop :: GamePlayClass s a g m p => p -> g -> s -> m (Maybe a, s)
gameLoop p g s0 = gameStep p s0 >>= gameLoop p g . evolveWithInput g
