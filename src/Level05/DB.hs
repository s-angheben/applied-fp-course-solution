{-# LANGUAGE OverloadedStrings #-}
module Level05.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Bifunctor                     (first)
import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection,
                                                     Query (fromQuery))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level05.Types                      (Comment, CommentText,
                                                     Error (DBError), Topic,
                                                     fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           Level05.AppM                       (AppM (AppM), liftEither)
import qualified Database.SQLite.SimpleErrors as DB
import Level05.DB.Types (DBComment(DBComment))
import qualified Control.Lens as Sql
import Data.Either (rights)
import Data.Colour.Names (wheat)
import Control.Lens (_18')

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

-- type DatabaseResponse a = Either SQLiteResponse a 

runDB
  :: (a -> Either Error b)
  -> IO a
  -> AppM b
runDB handler action = do
    resp <- liftIO $ DB.runDBAction action
    a <- liftEither (first DBError resp)
    liftEither (handler a)

  -- This function is intended to abstract away the running of DB functions and
  -- the catching of any errors. As well as the process of running some
  -- processing function over those results.

  -- Move your use of DB.runDBAction to this function to avoid repeating
  -- yourself in the various DB functions.

getComments
  :: FirstAppDB
  -> Topic
  -> AppM [Comment]
getComments appDb t = runDB catch1 action
  where
      catch1 = mapM fromDBComment          -- Fails if a single conversion goes wrong
      catch2 = rights . map fromDBComment  -- return the good conversion, don't fails
      action = Sql.query (dbConn appDb) sql (Sql.Only (getTopic t)) :: IO [DBComment]
      sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> AppM ()
addCommentToTopic appDb t ct = runDB return action
  where
    action = do 
              time <- getCurrentTime 
              Sql.execute (dbConn appDb) sql (getTopic t, getCommentText ct, time)
    sql  = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"

getTopics
  :: FirstAppDB
  -> AppM [Topic]
getTopics appDb = runDB return action
  where
    action = Sql.query_ (dbConn appDb) sql :: IO [Topic]
    sql = "SELECT DISTINCT topic FROM comments"

deleteTopic
  :: FirstAppDB
  -> Topic
  -> AppM ()
deleteTopic appDb t = runDB pure action
  where
    action = Sql.execute (dbConn appDb) sql (Sql.Only $ getTopic t)
    sql = "DELETE FROM comments WHERE topic = ?"

-- Go to 'src/Level05/Core.hs' next.
