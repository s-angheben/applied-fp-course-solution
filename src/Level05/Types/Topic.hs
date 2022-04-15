module Level05.Types.Topic
  (Topic
  , mkTopic
  , getTopic
  , encodeTopic
  ) where

import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E

import           Level05.Types.Error        (Error (EmptyTopic), nonEmptyText)

import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)
import qualified Database.SQLite.Simple as Sql

newtype Topic = Topic Text
  deriving Show

encodeTopic :: Applicative f => Encoder f Topic
encodeTopic = getTopic >$< E.text

mkTopic
  :: Text
  -> Either Error Topic
mkTopic =
  nonEmptyText Topic EmptyTopic

getTopic
  :: Topic
  -> Text
getTopic (Topic t) =
  t

instance Sql.FromRow Topic where
  fromRow = Topic <$> Sql.field