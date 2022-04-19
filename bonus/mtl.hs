#line 59 "mtl.md"
{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Applicative (liftA2)
import Data.Bifunctor (Bifunctor(..))
import Data.Either (either)
#line 115 "mtl.md"
-- Exercise: What are the constraints on the Functor instance? Check with `:i`.
newtype AppM' m e a = AppM' { runAppM' :: m (Either e a) } deriving Functor

instance Applicative m => Applicative (AppM' m e) where
  pure = AppM' . pure . pure
  AppM' f <*> AppM' a = AppM'$ liftA2 (<*>) f a

-- Exercise: Implement this
instance Monad m => Monad (AppM' m e) where
  (>>=) :: AppM' m e a -> (a -> AppM' m e b) -> AppM' m e b
  (>>=) (AppM' m) f = AppM' $ m >>= either (pure . Left) (runAppM' . f)   

#line 147 "mtl.md"
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
  deriving Functor
#line 155 "mtl.md"
instance Applicative m => Applicative (ExceptT e m) where
  pure = ExceptT . pure . pure
  ExceptT f <*> ExceptT a = ExceptT $ liftA2 (<*>) f a

instance Monad m => Monad (ExceptT e m) where
  ExceptT m >>= f = ExceptT $ m >>= either (pure . Left) (runExceptT . f)
#line 174 "mtl.md"
-- From Data.Functor.Identity
newtype Identity a = Identity { runIdentity :: a } deriving Functor

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
  Identity a >>= f = f a

-- Type aliases are eta-reduced as far as possible, for maximum usefulness.
-- (GHC can't expand a type alias until it's fully applied.)
type Except e = ExceptT e Identity
type Reader r = ReaderT r Identity
#line 200 "mtl.md"
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a } deriving Functor

-- Excercise: Write the Applicative and Monad instances
instance Applicative m => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure = ReaderT . pure . pure

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (<*>) (ReaderT f) (ReaderT a) = ReaderT $ liftA2 (<*>) f a

instance Monad m => Monad (ReaderT r m) where
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (>>=) (ReaderT m) f = ReaderT $ \r -> do
    a <- m r
    runReaderT (f a) r
#line 238 "mtl.md"
-- Exercise: implement all of these.

-- Return the environment.
ask :: Monad m => ReaderT r m r
--ask = ReaderT $ \r -> return r
ask = ReaderT pure

-- Apply a function to the environment, and return it.
asks :: Monad m => (r -> a) -> ReaderT r m a
-- asks = ReaderT $ \r -> return (f r)
asks f = f <$> ask 

-- Run a subcomputation in a modified environment.
-- This is a specialisation of withReaderT.
local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local = withReaderT 

-- Transform the environment of a reader.
withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f (ReaderT m) = ReaderT $ \r' -> m (f r')  

throwError :: Applicative m => e -> ExceptT e m a
throwError = ExceptT . pure . Left

catchError :: Monad m => ExceptT e m a -> (e -> ExceptT e m a) -> ExceptT e m a
catchError (ExceptT m) handler = ExceptT $ m >>= either (runExceptT . handler) (pure . Right)

-- Transform the error type.
withExceptT :: Functor m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
withExceptT f (ExceptT m) = ExceptT $ (first f) <$> m
-- withExceptT f = ExceptT . fmap (first f) . runExceptT

-- Transform the unwrapped computation.
mapExceptT
  :: (m (Either e a) -> n (Either e' b)) -> ExceptT e m a -> ExceptT e' n b
mapExceptT f (ExceptT m) = ExceptT $ f m

-- Lift a "catchError"-shaped function through a ReaderT.
-- We will need this later when we introduce MTL typeclasses.
liftCatch
  :: (m a -> (e -> m a) -> m a)
  -> ReaderT r m a -> (e -> ReaderT r m a) -> ReaderT r m a
liftCatch catch (ReaderT m) f = ReaderT $ \r -> 
  catch (m r) (\e -> runReaderT (f e) r)
#line 329 "mtl.md"
-- MonadTrans has the following laws, which show that `t` does indeed transform
-- `m`, and does so in a way that doesn't use the features of `t`:
--
-- 1. lift . pure = pure
-- 2. lift (m >>= f) = lift m >>= lift . f
class MonadTrans t where
  lift :: Monad m => m a -> t m a

-- Exercise: Write MonadTrans instances for `ReaderT r` and `ExceptT e`.
instance MonadTrans (ReaderT r) where
  lift :: m a -> ReaderT r m a
  lift = ReaderT . const 
--  lift m = ReaderT $ \r -> m

instance MonadTrans (ExceptT e) where
  lift :: (Monad m) => m a -> ExceptT e m a
  lift = ExceptT . fmap Right
#line 365 "mtl.md"
class Monad m => MonadIO m where
  liftIO :: IO a -> m a

-- Exercise: Write MonadIO instances for `ReaderT r m` and `ExceptT e m`,
-- assuming `MonadIO m`. Use `lift` instead of plumbing through the
-- transformers explicitly.
instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  liftIO = lift . liftIO 

instance MonadIO m => MonadIO (ExceptT e m) where
  liftIO :: IO a -> ExceptT r m a
  liftIO = lift . liftIO
#line 423 "mtl.md"
-- The functions in MTL do not have the trailing prime symbol (`'`),
-- but I must avoid duplicate definitions.
class Monad m => MonadReader r m | m -> r where
  ask' :: m r
  asks' :: (r -> a) -> m a
  local' :: (r -> r) -> m a -> m a

class Monad m => MonadError e m | m -> e where
  throwError' :: e -> m a
  catchError' :: m a -> (e -> m a) -> m a

-- Exercise: write these four instances.
--
-- The first two instances implement the class operations for each transformer.
-- You have written these functions already.
instance Monad m => MonadReader r (ReaderT r m) where
  ask' = ask
  asks' = asks
  local' = local

instance Monad m => MonadError e (ExceptT e m) where
  throwError' = throwError
  catchError' = catchError

-- These two instances lift the functionality of one
-- transformer through another.
instance MonadReader r m => MonadReader r (ExceptT e m) where
  ask' = lift . ask 
  asks' = lift . asks
  local' = lift . local

instance MonadError e m => MonadError e (ReaderT r m) where
  throwError' = lift . throwError
  catchError' = liftCatch . catchError
#line 515 "mtl.md"
data Env = Env -- Dummy type for the sake of example

-- This is the Level07 AppM, which also passes around an environment
-- (Look ma, no handwritten instances!)
newtype AppM e a = AppM (ReaderT Env (ExceptT e IO) a)
  deriving (Functor, Applicative, Monad, MonadError e, MonadReader Env)

runAppM :: AppM e a -> Env -> IO (Either e a)
runAppM (AppM m) env = runExceptT $ runReaderT m env
