{-# LANGUAGE OverloadedStrings #-}
module Level08.Core
  ( runApplication
  , prepareAppReqs
  , app
  ) where

import           Control.Applicative                (liftA2)
import qualified Control.Exception                  as Ex
import           Control.Monad                      (join)

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Network.Wai                        (Application, Request,
                                                     Response, pathInfo,
                                                     requestMethod,
                                                     strictRequestBody)
import           Network.Wai.Handler.Warp           (run)

import           Data.Bifunctor                     (first)
import           Data.Either                        (Either (Left, Right),
                                                     either)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Data.Text.Encoding                 (decodeUtf8)
import           Data.Text.IO                       (hPutStrLn)

import qualified Data.ByteString.Lazy.Char8         as LBS

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           System.IO                          (stderr)

import qualified Waargonaut.Encode                  as E

import qualified Level08.Conf                       as Conf
import qualified Level08.DB                         as DB

import qualified Level08.Responses                  as Res
import           Level08.Types                      (Conf (dbFilePath), ConfigError,
                                                     ContentType (PlainText),
                                                     Error (..), RqType (..),
                                                     confPortToWai,
                                                     encodeComment, encodeTopic,
                                                     mkCommentText, mkTopic, DBFilePath (getDBFilePath))

import           Level08.AppM                       (App, Env (..),
                                                     runApp)

import           Control.Monad.Except               (ExceptT (..), runExceptT, MonadError (throwError), withExcept, liftEither)
import Level08.Conf (parseOptions)
import Level08.DB
import Control.Error (withExceptT)
import qualified Data.Text as T
import Control.Lens (_18')
import Level08.Responses (mkResponse)

data StartUpError
  = DBInitErr SQLiteResponse
  | ConfErr ConfigError
  deriving Show

runApplication :: IO ()
runApplication = do
  appE <- runExceptT prepareAppReqs
  either print runWithDBConn appE
  where
    runWithDBConn env =
      appWithDB env >> DB.closeDB (envDB env)

    appWithDB env = Ex.finally
      (run ( confPortToWai . envConfig $ env ) (app env))
      $ DB.closeDB (envDB env)

prepareAppReqs :: ExceptT StartUpError IO Env
prepareAppReqs = do
  conf  <- withExceptT ConfErr tryParseConf
  appDB <- withExceptT DBInitErr $ tryInitDB (dbFilePath conf)
  return $ Env loggerF conf appDB
  where 
    tryParseConf   = ExceptT $ parseOptions "files/appconfig.json"
    tryInitDB dbfp = ExceptT $ initDB dbfp
    loggerF = liftIO . putStrLn . ("Logger: " ++) . T.unpack
  
app
  :: Env
  -> Application
app env req resp = runApp myApp env >>= resp . handleEspErr
  where
    myApp = mkRequest req >>= handleRequest 
    handleEspErr = either mkErrorResponse id 
  

handleRequest
  :: RqType
  -> App Response
handleRequest rqType = case rqType of
  AddRq t c -> Res.resp200 PlainText "Success" <$ DB.addCommentToTopic t c
  ViewRq t  -> Res.resp200Json (E.list encodeComment) <$> DB.getComments t
  ListRq    -> Res.resp200Json (E.list encodeTopic)   <$> DB.getTopics

mkRequest
  :: Request
  -> App RqType
mkRequest rq =
  liftEither =<< case ( pathInfo rq, requestMethod rq ) of
    -- Commenting on a given topic
    ( [t, "add"], "POST" ) -> liftIO (mkAddRequest t <$> strictRequestBody rq)
    -- View the comments on a given topic
    ( [t, "view"], "GET" ) -> pure ( mkViewRequest t )
    -- List the current topics
    ( ["list"], "GET" )    -> pure mkListRequest
    -- Finally we don't care about any other requests so throw your hands in the air
    _                      -> pure ( Left UnknownRoute )

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest ti c = AddRq
  <$> mkTopic ti
  <*> (mkCommentText . decodeUtf8 . LBS.toStrict) c

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest =
  fmap ViewRq . mkTopic

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse UnknownRoute     =
  Res.resp404 PlainText "Unknown Route"
mkErrorResponse EmptyCommentText =
  Res.resp400 PlainText "Empty Comment"
mkErrorResponse EmptyTopic       =
  Res.resp400 PlainText "Empty Topic"
mkErrorResponse ( DBError _ )    =
  Res.resp500 PlainText "OH NOES"
