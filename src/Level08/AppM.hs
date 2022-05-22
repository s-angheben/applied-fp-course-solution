{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Level08.AppM
  ( AppM (..)
  , App
  , Env (..)
  , runApp
  ) where

import           Control.Monad.Except   (MonadError (..), ExceptT, MonadTrans (lift))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (..), ReaderT)

import           Data.Text              (Text)

import           Level08.Types          (Conf, FirstAppDB)
import           Level08.Types.Error    (Error)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

data Env = Env
  { envLoggingFn :: Text -> App ()
  , envConfig    :: Conf
  , envDB        :: FirstAppDB
  }

newtype AppM e a = AppM (ReaderT Env (ExceptT e IO) a)
  deriving (Functor, Applicative, Monad, MonadError e, MonadReader Env, MonadIO)

runAppM :: AppM e a -> Env -> IO (Either e a)
runAppM (AppM m) env = runExceptT $ runReaderT m env

type App = AppM Error

runApp :: App a -> Env -> IO (Either Error a)
runApp = runAppM


