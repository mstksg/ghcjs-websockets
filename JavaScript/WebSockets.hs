{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module JavaScript.WebSockets (
    -- * Types and Classes
    Connection          -- abstract
  , ConnectionProcess   -- abstract, instance: Functor, Applicative, Monad, MonadIO
  , Sendable
    -- * Opening, closing, running connections
  , withUrl             -- :: Text -> ConnectionProcess a -> IO a
  , withConn            -- :: Connection -> ConnectionProcess a -> IO a
  , openConnection      -- :: Text -> IO Connection
  , closeConnection     -- :: Connection -> IO ()
    -- * Sending messages
  , sendText            -- :: Text -> ConnectionProcess ()
  , sendBinary          -- :: Binary a => a -> ConnectionProcess ()
  , send                -- :: Sendable a => a -> ConnectionProcess ()
  , sendTagged          -- :: (Binary a, Typeable a) => ConnectionProcess ()
    -- * Receiving messages
  , expectText          -- :: ConnectionProcess Text
  , expectBS            -- :: ConnectionProcess ByteString
  , expectEither        -- :: Binary a => ConnectionProcess (Either ByteString a)
  , expectMaybe         -- :: Binary a => ConnectionProcess (Maybe a)
  , expect              -- :: Binary a => ConnectionProcess a
  , expectTagged        -- :: (Binary a, Typeable a) => ConnectionProcess a
    -- * Inspecting connections
  , selfConn            -- :: ConnectionProcess Connection
  , connOrigin          -- :: Connection -> Text
  ) where

import Control.Applicative            ((<$>))
import Control.Exception              (bracket)
import Control.Spoon                  (teaspoon)
import Data.Binary                    (Binary, encode, decode)
import Data.Binary.Tagged
import Data.ByteString.Lazy           (ByteString, fromStrict)
import Data.Foldable                  (mapM_)
import Data.Text                      (Text)
import Data.Text.Encoding             (encodeUtf8, decodeUtf8)
import Data.Typeable                  as DT
import JavaScript.WebSockets.Internal
import Prelude hiding                 (mapM_)

-- | 'Sendable' basically adds a convenient but not exactly necessary layer
-- of abstraction over 'sendText' and 'sendBinary'.  You can send both
-- 'Text' and 'Binary' instances using 'send'.  You really should never
-- have to define your own instances.
class Sendable s where
    encodeSendable :: s -> ByteString

instance Sendable Text where
    encodeSendable = fromStrict . encodeUtf8

instance Binary a => Sendable a where
    encodeSendable = encode

-- | Make a connection to the websocket server given by the url and
-- execute/run a 'ConnectionProcess' process/computation with that
-- connection.  Handles the closing and stuff for you.
withUrl :: Text -> ConnectionProcess a -> IO a
withUrl url process = do
    bracket
      (openConnection url)
      (closeConnection)
      (flip withConn process)

-- | Send strict 'Text' through the connection.
sendText :: Text -> ConnectionProcess ()
sendText = send

-- | Send an instance of 'Binary' through the connection.  It will be
-- serialized using 'encode' before being sent.
sendBinary :: Binary a => a -> ConnectionProcess ()
sendBinary = send

-- | Send a lazy 'ByteString' through the connection.
sendBS :: ByteString -> ConnectionProcess ()
sendBS bs = ProcessSend bs (return ())

-- | Send data tagged with 'Data.Binary.Tagged' --- basically, send the
-- serialized data tagged with information about its type.  See
-- 'Data.Binary.Tagged' in the tagged-binary package for more information.
-- Allows you to treat the channel as a polymorphic dynamic communication
-- channel, and the server can chose to accept, ignore, or queue the
-- message based on its type.
sendTagged :: (Binary a, Typeable a) => a -> ConnectionProcess ()
sendTagged = sendBS . encodeTagged

-- | Send a 'Sendable' instance --- either 'Text' or an instance of
-- 'Binary'.  Mostly a convenience function abstracting over 'sendText' and
-- 'sendBinary'.
send :: Sendable s => s -> ConnectionProcess ()
send = sendBS . encodeSendable

expectBS :: ConnectionProcess ByteString
expectBS = ProcessExpect return

expectEither :: Binary a => ConnectionProcess (Either ByteString a)
expectEither = do
  bs <- expectBS
  return $ maybe (Left bs) Right . teaspoon . decode $ bs

expectMaybe :: Binary a => ConnectionProcess (Maybe a)
expectMaybe = do
  expected <- expectEither
  case expected of
    Right x -> return (Just x)
    Left bs -> do
      mapM_ (flip queueUpFp bs) (bsFingerprint bs)
      return Nothing

expect :: Binary a => ConnectionProcess a
expect = do
  res <- expectMaybe
  case res of
    Just res' -> return res'
    Nothing   -> expect

expectText :: ConnectionProcess Text
expectText = decodeUtf8 <$> expect

expectTagged :: forall a. (Binary a, Typeable a) => ConnectionProcess a
expectTagged = do
  -- check queue first
  let fp = typeFingerprint (undefined :: a)
  queued <- popQueueFp fp
  case queued of
    -- something is there!
    Just q  ->
      case decodeTagged q of
        Just a -> return a
        Nothing -> error "Unable to decode tagged ByteString"
    -- otherwise...
    Nothing -> do
      bs <- expectBS
      case bsFingerprint bs of
        Just fpIn
          | fpIn == fp ->
              case decodeTagged bs of
                Just a  -> return a
                Nothing -> error "Unable to decode tagged ByteString"
          | otherwise -> do
              queueUpFp fpIn bs
              expectTagged
        Nothing   -> expectTagged


connOrigin :: Connection -> Text
connOrigin = _connOrigin

