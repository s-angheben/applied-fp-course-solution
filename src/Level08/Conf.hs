{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Level08.Conf
    ( parseOptions
    ) where

import           GHC.Word                  (Word16)

import           Data.Bifunctor            (first)
import           Data.Semigroup            (Last (..), (<>))

import           Level08.Types            (Conf (..), ConfigError (..),
                                            DBFilePath (DBFilePath),
                                            PartialConf (..), Port (Port))

import           Level08.Conf.CommandLine (commandLineParser)
import           Level08.Conf.File        (parseJSONConfigFile)

defaultConf
  :: PartialConf
defaultConf = PartialConf
  (pure (Last $ Port 3000))
  (pure (Last $ DBFilePath "app_db.db"))

makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig pc = Conf
  <$> lastToEither MissingPort pcPort
  <*> lastToEither MissingDBFilePath pcDBFilePath
  where
    lastToEither
      :: ConfigError
      -> (PartialConf -> Maybe (Last b))
      -> Either ConfigError b
    lastToEither e g =
      maybe (Left e) (Right . getLast) . g $ pc 

parseOptions
  :: FilePath
  -> IO (Either ConfigError Conf)
parseOptions fp =
  let mkCfg cli file = makeConfig (defaultConf <> file <> cli)
  in do
    cli' <- commandLineParser
    ( >>= mkCfg cli' ) <$> parseJSONConfigFile fp
