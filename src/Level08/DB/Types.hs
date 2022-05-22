module Level08.DB.Types where

import           Data.Text                      (Text)
import           Data.Time                      (UTCTime)

import           Database.SQLite.Simple.FromRow (FromRow (fromRow), field)

data DBComment = DBComment
  { dbCommentId      :: Int
  , dbCommentTopic   :: Text
  , dbCommentComment :: Text
  , dbCommentTime    :: UTCTime
  }
  deriving Show

instance FromRow DBComment where
  fromRow = DBComment
            <$> field
            <*> field
            <*> field
            <*> field
