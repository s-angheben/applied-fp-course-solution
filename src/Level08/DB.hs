{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Level08.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks, MonadReader (reader))

import           Data.Bifunctor                     (first)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow,
                                                     Query (fromQuery), ToRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level08.AppM                      (App, Env (envDB))

import           Level08.Types                     (Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Error (DBError),
                                                     FirstAppDB (FirstAppDB, dbConn),
                                                     Topic, fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)
import Control.Monad.Except

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: DBFilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  con <- Sql.open ( getDBFilePath fp )
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn
  :: App Connection
getDBConn = asks (dbConn . envDB)


runDB
  :: (a -> Either Error b)
  -> (Connection -> IO a)
  -> App b
runDB h action = do
  conn <- getDBConn
-- type DatabaseResponse a = Either SQLiteResponse a 
  a <- liftIO $ first DBError <$> Sql.runDBAction (action conn)
  liftEither $ a >>= h

getComments
  :: Topic
  -> App [Comment]
getComments t = do
  let q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  runDB (traverse fromDBComment) (\conn -> Sql.query conn q (Sql.Only . getTopic $ t))

  

addCommentToTopic
  :: Topic
  -> CommentText
  -> App ()
addCommentToTopic t c = do
  nowish <- liftIO getCurrentTime
  let q = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  runDB Right (\conn -> Sql.execute conn q (getTopic t, getCommentText c, nowish))

getTopics
  :: App [Topic]
getTopics =
  let q = "SELECT DISTINCT topic FROM comments"
  in runDB (traverse ( mkTopic . Sql.fromOnly )) (\conn -> Sql.query_ conn q)

deleteTopic
  :: Topic
  -> App ()
deleteTopic t =
  let q = "DELETE FROM comments WHERE topic = ?"
  in runDB Right (\conn -> Sql.execute conn q (Sql.Only . getTopic $ t))


