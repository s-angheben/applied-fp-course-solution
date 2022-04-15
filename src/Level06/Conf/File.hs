{-# LANGUAGE OverloadedStrings #-}
module Level06.Conf.File where

import           Data.ByteString            (ByteString, readFile)

import           Data.Text                  (Text, pack)

import           Data.Bifunctor             (first)
import           Data.Monoid                (Last (Last))

import           Control.Exception          (try, IOException)

import qualified Data.Attoparsec.ByteString as AB

import           Waargonaut                 (Json)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (DecodeError (ParseFailed), AsDecodeError (_ConversionFailure))

import           Level06.AppM               (AppM (runAppM), liftIO, liftEither, throwError, bimap)
import           Level06.Types              (ConfigError (BadConfFile, ReadFileError),
                                             PartialConf (PartialConf), partialConfDecoder)
import qualified Data.ByteString as AB
import qualified Network.ByteOrder as Data
import qualified Data.ByteString as B
-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to write a
-- 'waargonaut' 'Decoder' to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> runAppM $ readConfFile "badFileName.no"
-- Left (<YourErrorConstructorHere> "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> runAppM $ readConfFile "files/test.json"
-- Right "{\n  \"foo\": 33\n}\n"
--
readConfFile
  :: FilePath
  -> AppM ConfigError ByteString
readConfFile fp = do
      ea <- liftIO read
      case ea of
        Left _  -> throwError ReadFileError
        Right v -> return v
      where
        read :: IO (Either IOException ByteString)
        read = try (Data.ByteString.readFile fp)

--    let a = liftIO $ try $ Data.ByteString.readFile fp
--    in a

  -- Reading a file may throw an exception for any number of
  -- reasons. Use the 'try' function from 'Control.Exception' to catch
  -- the exception and turn it into an error value that is thrown as
  -- part of our 'AppM' transformer.
  --
  -- No exceptions from reading the file should escape this function.
  --
--  error "readConfFile not implemented"

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> AppM ConfigError PartialConf
parseJSONConfigFile fp = do
  file <- readConfFile fp
  first (BadConfFile . fst) $ liftEither (decode file)  
  where
    decode file = D.pureDecodeFromByteString AB.parseOnly partialConfDecoder file

-- Go to 'src/Level06/Conf.hs' next.
