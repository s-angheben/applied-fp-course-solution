#line 28 "mtl.md"
{-# OPTIONS_GHC -Wall -Wno-unused-imports -Wno-unused-matches #-}

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import qualified Data.Semigroup as Semigroup
import Data.Kind (Type)
import Data.Word (Word16)
#line 52 "mtl.md"
newtype Port = Port Word16
  deriving (Read, Show)

data Conf = Conf
  { port       :: Port
  , dbFilePath :: FilePath
  } deriving Show

data PartialConf = PartialConf
  { pcPort       :: Maybe (Semigroup.Last Port)
  , pcDBFilePath :: Maybe (Semigroup.Last FilePath)
  }
#line 80 "mtl.md"
data Config f = Config
  { _port :: f Port
  , _dbFilePath :: f FilePath
  }

-- It is sometimes useful to provide a convenience function that uses
-- pure to add all the `f` wrappers.
-- Exercise: implement it.
config :: Applicative f => Port -> FilePath -> Config f
config port fp = Config (pure port) (pure fp) 

-- These declarations use the -XStandaloneDeriving extension, which lets us
-- write automatically-derivable instances away from the data declaration.
-- We need to do this here because GHC fails to work out which constraints
-- we require in the context of the instance.
-- Still, we get the Eq and Show instances for minimal boilerplate.
deriving instance (Eq (f Port), Eq (f FilePath)) => Eq (Config f)
deriving instance (Show (f Port), Show (f FilePath)) => Show (Config f)
#line 117 "mtl.md"
-- Conf' and PartialConf' are primed to not clash with the names from level 7.
type Conf' = Config Identity

-- This is Data.Maybe.Last, which will eventually be deprecated and removed.
-- Using `Maybe (Last a)` and the `Compose` newtype is the forward-compatible
-- recommendation but we can't do that here just yet;
-- see https://gitlab.haskell.org/ghc/ghc/issues/17859 for the gory details.
newtype Last a = Last { getLast :: Maybe a }

instance Semigroup (Last a) where
  Last (Just a) <> Last Nothing = Last $ Just a
  _ <> b = b

instance Monoid (Last a) where
  mempty = Last Nothing

type PartialConf' = Config Last

-- Exercise: implement these instances
instance (Semigroup (f Port), Semigroup (f FilePath)) => Semigroup (Config f) where
  (<>) (Config ap afp) (Config ab bfp) = Config (ap <> ab) (afp <> bfp) 

instance (Monoid (f Port), Monoid (f FilePath)) => Monoid (Config f) where
  mempty = Config mempty mempty 
#line 180 "mtl.md"
-- Exercise: implement this
fromPartialConf' :: PartialConf' -> Maybe Conf'
fromPartialConf' (Config (Last mp) (Last mdb)) = config <$> mp <*> mdb
#line 230 "mtl.md"
-- This is (roughly) Rank2.Functor from the `rank2classes` package.
class Rank2Functor (g :: (Type -> Type) -> Type) where
  r2fmap :: (forall a . p a -> q a) -> g p -> g q

instance Rank2Functor Config where
  r2fmap :: (forall a . p a -> q a) -> Config p -> Config q
  r2fmap f (Config p fp) = Config (f p) (f fp)
#line 281 "mtl.md"
-- This is (roughly) `Rank2.Traversable` from `rank2classes`.
class Rank2Functor g => Rank2Traversable g where
  {-# MINIMAL r2sequence | r2traverse #-}

  -- The type of 'sequence' contains a 'Compose' because we need to make
  -- sure there's a functor underneath the `m` we use for our effects.
  -- (Even if that underlying functor is something boring like `Identity`.)
  -- Exercise: implement 'sequence' using 'traverse'.
  r2sequence :: Applicative m => g (Compose m p) -> m (g p)
  r2sequence = r2traverse getCompose

  -- Exercise: implement 'traverse' using 'sequence'.
  r2traverse :: Applicative m => (forall a . p a -> m (q a)) -> g p -> m (g q)
  r2traverse f = r2sequence . r2fmap (Compose . f)

-- Exercise: implement both methods of Rank2Traversable.
instance Rank2Traversable Config where
  r2sequence :: Applicative m => Config (Compose m p) -> m (Config p)
  r2sequence (Config mp mdb) = Config <$> getCompose mp <*> getCompose mdb

  r2traverse
    :: Applicative m
    => (forall a . p a -> m (q a))
    -> Config p
    -> m (Config q)
  r2traverse f (Config mp mdb) = Config <$> (f mp) <*> (f mdb) 
#line 342 "mtl.md"
-- Exercise: implement this
fromLast :: Rank2Traversable g => g Last -> Maybe (g Identity)
fromLast = r2traverse $ fmap Identity . getLast

--- tests
-- config IO
type ConfIO = Config IO 

readConfig :: ConfIO
readConfig = Config readPort readFp
  where
    readPort :: IO Port
    readPort = Port . (read :: String -> Word16) <$> getLine 

    readFp :: IO FilePath
    readFp = getLine



fp :: FilePath
fp = "testFp"