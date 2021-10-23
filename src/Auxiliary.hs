module Auxiliary(
    ReaderT(..),
    WriterT(..),
    StateT(..),
    local,
    tell,
    liftWriterT,
    liftWriterTFunc,
    pop,
    getHead,
    headSafe,
    tailSafe
) where
import Prelude
import Control.Applicative (Applicative, Alternative (empty), (<|>))
import Data.Bifunctor (first)


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

liftWriterTFunc :: (Monoid w, Monad m) => (m (a, w) -> m (a, w)) -> WriterT w m a -> WriterT w m a
liftWriterTFunc f (WriterT mx) = WriterT $ f mx
     
---------------------------------- StateT ------------------------------------

newtype StateT s m a = StateT{runStateT :: s -> m (a,s)}

instance Monad m => Functor (StateT s m) where
    fmap f (StateT fa) = StateT $ \s -> do
        (xa, sx) <- fa s
        return (f xa, sx)

instance Monad m => Applicative (StateT s m) where
    pure x = StateT $ \s -> return (x, s)
    StateT ff <*> StateT fa = StateT $ \s -> do
        (f, s')   <- ff s
        (xa, s'') <- fa s'
        return (f xa, s'')


instance Monad m => Monad (StateT s m) where
    return = pure 
    StateT fa >>= f = StateT $ \s -> do
        (xa, xs) <- fa s
        (xb, sb) <- runStateT (f xa) xs
        return (xb, sb)

----------------------- Particular StateT ------------------------

pop :: Monad m => StateT [a] m ()
pop = StateT f where
    f [] = return ((),[])
    f (_:xs) = return ((), xs)

getHead :: Monad m => StateT [a] m (Maybe a)
getHead = StateT f where
    f [] = return (Nothing, []) 
    f s@(x:_) = return (Just x, s)

headSafe :: [a] -> Maybe a
headSafe [] = Nothing 
headSafe (x:_) = Just x

tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe x = tail x