module Auxiliary(
    ReaderT(..)
) where
import Prelude 
import Control.Applicative (Applicative, Alternative (empty), (<|>))

newtype ReaderT r m a = ReaderT{runReaderT :: r -> m a}

instance Functor m => Functor (ReaderT r m) where     
    fmap f (ReaderT g) = ReaderT $ \r -> fmap f (g r)

instance Applicative m => Applicative (ReaderT r m) where
    pure x = ReaderT $ const (pure x)
    (ReaderT ff)<*>(ReaderT fa) = ReaderT $ \r -> ff r <*> fa r

instance Alternative m => Alternative (ReaderT r m) where
    empty = ReaderT $ const empty
    ReaderT fa <|> ReaderT fb = ReaderT $ \r -> fa r <|> fb r

instance Monad m => Monad (ReaderT r m) where
    return = pure
    (ReaderT fa) >>= f = ReaderT $ \r -> do
        ax <- fa r
        runReaderT (f ax) r

