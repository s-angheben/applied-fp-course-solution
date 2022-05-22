{-# LANGUAGE OverloadedStrings #-}
module Level08.Types
  ( Error (..)
  , ConfigError (..)
  , PartialConf (..)
  , Port (..)
  , DBFilePath (..)
  , Conf (..)
  , FirstAppDB (..)
  , RqType (..)
  , ContentType (..)
  , Comment (..)
  , Topic
  , CommentText
  , partialConfDecoder
  , mkTopic
  , getTopic
  , mkCommentText
  , getCommentText
  , renderContentType
  , fromDBComment
  , confPortToWai
  , encodeComment
  , encodeTopic
  ) where

import           System.IO.Error                    (IOError)

import           GHC.Word                           (Word16)

import           Data.ByteString                    (ByteString)
import           Data.Text                          (pack)

import           Data.Functor.Contravariant         ((>$<))
import           Data.Semigroup                     (Last (Last), Semigroup ((<>)))

import           Data.Time                          (UTCTime)
import qualified Data.Time.Format                   as TF

import           Waargonaut.Decode                  (CursorHistory, Decoder)
import qualified Waargonaut.Decode                  as D
import           Waargonaut.Decode.Error            (DecodeError)

import           Waargonaut.Encode                  (Encoder)
import qualified Waargonaut.Encode                  as E

import           Database.SQLite.Simple             (Connection)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level08.DB.Types                   (DBComment (dbCommentComment, dbCommentId, dbCommentTime, dbCommentTopic))

import           Level08.Types.Error                (Error (DBError, EmptyCommentText, EmptyTopic, UnknownRoute))

import           Level08.Types.CommentText          (CommentText,
                                                     encodeCommentText,
                                                     getCommentText,
                                                     mkCommentText)

import           Level08.Types.Topic                (Topic, encodeTopic,
                                                     getTopic, mkTopic)

newtype CommentId = CommentId Int
  deriving (Show)

encodeCommentId :: Applicative f => Encoder f CommentId
encodeCommentId = (\(CommentId i) -> i) >$< E.int

data Comment = Comment
  { commentId    :: CommentId
  , commentTopic :: Topic
  , commentText  :: CommentText
  , commentTime  :: UTCTime
  }
  deriving Show

encodeISO8601DateTime :: Applicative f => Encoder f UTCTime
encodeISO8601DateTime = pack . TF.formatTime tl fmt >$< E.text
  where
    fmt = TF.iso8601DateFormat (Just "%H:%M:%S")
    tl = TF.defaultTimeLocale { TF.knownTimeZones = [] }

encodeComment :: Applicative f => Encoder f Comment
encodeComment = E.mapLikeObj $ \c ->
  E.atKey' "id"    encodeCommentId       (commentId c) .
  E.atKey' "topic" encodeTopic           (commentTopic c) .
  E.atKey' "text"  encodeCommentText     (commentText c) .
  E.atKey' "time"  encodeISO8601DateTime (commentTime c)

fromDBComment
  :: DBComment
  -> Either Error Comment
fromDBComment dbc =
  Comment (CommentId     $ dbCommentId dbc)
      <$> (mkTopic       $ dbCommentTopic dbc)
      <*> (mkCommentText $ dbCommentComment dbc)
      <*> (pure          $ dbCommentTime dbc)

data RqType
  = AddRq Topic CommentText
  | ViewRq Topic
  | ListRq

data ContentType
  = PlainText
  | JSON

renderContentType
  :: ContentType
  -> ByteString
renderContentType PlainText = "text/plain"
renderContentType JSON      = "application/json"

-----------------
-- Config Types
-----------------

newtype Port = Port
  { getPort :: Word16 }
  deriving (Eq, Show)

newtype DBFilePath = DBFilePath
  { getDBFilePath :: FilePath }
  deriving (Eq, Show)


data Conf = Conf
  { port       :: Port
  , dbFilePath :: DBFilePath
  }
  deriving Eq

confPortToWai
  :: Conf
  -> Int
confPortToWai =
  fromIntegral . getPort . port

data ConfigError
  = BadConfFile (DecodeError, CursorHistory)
  | MissingPort
  | MissingDBFilePath
  | JSONDecodeError String
  | ConfigFileReadError IOError
  deriving Show

data PartialConf = PartialConf
  { pcPort       :: Maybe (Last Port)
  , pcDBFilePath :: Maybe (Last DBFilePath)
  }

instance Semigroup PartialConf where
  a <> b = PartialConf
    { pcPort       = pcPort a <> pcPort b
    , pcDBFilePath = pcDBFilePath a <> pcDBFilePath b
    }

partialConfDecoder :: Monad f => Decoder f PartialConf
partialConfDecoder = PartialConf
  <$> lastAt "port" D.integral Port
  <*> lastAt "dbFilePath" D.string DBFilePath
  where
    lastAt k d c = fmap (Last . c) <$> D.atKeyOptional k d

newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }
