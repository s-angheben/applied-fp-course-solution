{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Level06.Conf
    ( parseOptions
    ) where

import           GHC.Word                 (Word16)

import           Data.Bifunctor           (first)
import           Data.Monoid              ((<>))

import           Level06.AppM             (AppM, liftIO, liftEither, catchError)
import           Level06.Types            (Conf (Conf), ConfigError (MissingPathConfig, MissingPortConfig),
                                           DBFilePath (DBFilePath), PartialConf (PartialConf, pcPort, pcDBFilePath),
                                           Port (Port))

import           Level06.Conf.CommandLine (commandLineParser)
import           Level06.Conf.File        (parseJSONConfigFile)
import Data.Semigroup (Last(Last))

-- | For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf
  :: PartialConf
defaultConf =
  PartialConf (mkPort 3000) (mkFp ":memory:")
  where
    mkPort = Just . Last . Port 
    mkFp   = Just . Last . DBFilePath 

-- | We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig pc = 
  case mPort of
    Nothing       -> Left MissingPortConfig  
    Just (Last p) -> case mFp of
      Nothing        -> Left MissingPathConfig 
      Just (Last fp) -> Right $ Conf p fp
  where 
    mPort = pcPort pc
    mFp   = pcDBFilePath pc

  

-- | This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.
--
-- Remember that we want the command line configuration to take precedence over
-- the File configuration, so if we think about combining each of our ``Conf``
-- records. By now we should be able to write something like this:
--
-- ``defaults <> file <> commandLine``
--
parseOptions
  :: FilePath
  -> AppM ConfigError Conf
parseOptions fp = do
  commandLine <- liftIO commandLineParser 
  file <- catchError (parseJSONConfigFile fp) (handlerFile)
  liftEither $ makeConfig (defaultConf <> file <> commandLine)
  where
    handlerFile err = do
      liftIO $ putStrLn $ "error parsing the file config: " ++ show err
      return $ PartialConf Nothing Nothing 

  -- Parse the options from the config file: "files/appconfig.json"
  -- Parse the options from the commandline using 'commandLineParser'
  -- Combine these with the default configuration 'defaultConf'
  -- Return the final configuration value
--  error "parseOptions not implemented"
 