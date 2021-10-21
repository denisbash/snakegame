module Auxiliary(
    ReaderT(..),
    WriterT(..),
    local,
    tell,
    liftWriterT
) where
import Prelude
import Control.Applicative (Applicative, Alternative (empty), (<|>))
import Data.Bifunctor (first)
import Data.Complex (magnitude)

---------------- ReaderT ------------------------------

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

local :: Monad m => (r -> r) -> ReaderT r m a -> ReaderT r m a
local f (ReaderT g) = ReaderT $ g . f

---------------------- WriterT ------------------------------------

newtype WriterT w m a = WriterT{runWriterT :: m (a, w)}

instance Monad m => Functor (WriterT w m) where
    fmap f (WriterT mx) = WriterT $ first f<$>mx

instance (Monad m, Monoid w) => Applicative (WriterT w m) where
    pure x = WriterT $ return (x, mempty )
    WriterT mf <*> WriterT ma = WriterT $ do
        (f, wf)  <- mf 
        (ax, wa) <- ma
        return (f ax, wf `mappend` wa)

instance (Monad m, Alternative m, Monoid w) => Alternative (WriterT w m) where
    empty = WriterT empty
    (WriterT ma) <|> (WriterT mb) = WriterT (ma <|> mb)

instance (Monad m, Monoid w) => Monad (WriterT w m) where
     return = pure
     WriterT ma >>= f = WriterT $ do
         (xa, wa) <- ma
         (xb, wf) <- runWriterT $ f xa
         return (xb, wa `mappend` wf)


tell :: Monad m => w -> WriterT w m ()
tell wx = WriterT $ return ((), wx)

liftWriterT :: (Monoid w, Monad m) => m a -> WriterT w m a
liftWriterT mx = WriterT $ do
    x <- mx
    return (x, mempty)
     
--execWriter :: WriterT w m a -> w
--execWriter $ WriterT mx = 