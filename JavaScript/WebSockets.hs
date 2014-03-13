{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module JavaScript.WebSockets (
    -- * Types and Classes
    Connection          -- abstract
  , ConnectionProcess   -- abstract, instance: Functor, Applicative, Monad, MonadIO
  , Sendable            -- abstract
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
    -- * Receiving messages (monomorphic)
  , expectBS            -- :: ConnectionProcess ByteString
  , expectText          -- :: ConnectionProcess Text
  , expectEither        -- :: Binary a => ConnectionProcess (Either ByteString a)
  , expectMaybe         -- :: Binary a => ConnectionProcess (Maybe a)
  , expect              -- :: Binary a => ConnectionProcess a
    -- * Receiving messages (polymorphic)
  , expectTagged        -- :: (Binary a, Typeable a) => ConnectionProcess a
  , expectMaybe'        -- :: Binary a => ConnectionProcess (Maybe a)
  , expectText'         -- :: ConnectionProcess Text
  , expect'             -- :: Binary a => ConnectionProcess a
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
-- 'Data.Binary.Tagged' in the _tagged-binary_ package for more
-- information.  Allows you to treat the channel as a polymorphic dynamic
-- communication channel, and the server can chose to accept, ignore, or
-- queue the message based on its type.
sendTagged :: (Binary a, Typeable a) => a -> ConnectionProcess ()
sendTagged = sendBS . encodeTagged

-- | Send a 'Sendable' instance --- either 'Text' or an instance of
-- 'Binary'.  Mostly a convenience function abstracting over 'sendText' and
-- 'sendBinary'.
send :: Sendable s => s -> ConnectionProcess ()
send = sendBS . encodeSendable

-- | Block and wait for a 'ByteString' to come from the connection.
expectBS :: ConnectionProcess ByteString
expectBS = ProcessExpect return

-- | Block and wait for the next incoming (typed) message.  If the message
-- can be successfully decoded into the desired type, return 'Right x'.
-- Otherwise, return the 'ByteString' in a 'Left'.
--
-- This is polymorphic in its return type, so you should either use the
-- result later somehow or explicitly annotate the type so that GHC knows
-- what you want.
expectEither :: Binary a => ConnectionProcess (Either ByteString a)
expectEither = do
  bs <- expectBS
  return $ maybe (Left bs) Right . teaspoon . decode $ bs

-- | Block and wait for the next incoming (typed) message.  If the message
-- can be successfully decoded into a value of the desired type, return
-- 'Just x'.  Otherwise, return 'Nothing', and throw away the message.
--
-- This is polymorphic in its return type, so you should either use the
-- result later somehow or explicitly annotate the type so that GHC knows
-- what you want.
--
-- __Warning!__: Be aware that if the incoming message does not
-- successfully decode, the message is lost _forever_ with 'expectMaybe'.
-- If you want to utilize the dynamically typed polymorphic communication
-- channel feature together with monomorphic channels, use 'expectMaybe''
-- to maintain typed queues for non-decodable messages.
expectMaybe :: Binary a => ConnectionProcess (Maybe a)
expectMaybe = either (const Nothing) (Just) <$> expectEither

-- | Block and wait for the next incoming (typed) message.  If the message
-- can be successfully decoded into a value of the desired type, return
-- 'Just x'.  Otherwise, return 'Nothing'.
--
-- This is polymorphic in its return type, so you should either use the
-- result later somehow or explicitly annotate the type so that GHC knows
-- what you want.
--
-- Note that while this looks a lot like 'expectEither' and 'expectMaybe',
-- it actually has very different semantics.  If an item that is not
-- decoded successfully is actually a 'Tagged' item (from _tagged-binary_),
-- then it will be queued up with its type.  Later, when you use
-- 'expectTagged' and ask for an item with the type that the incoming
-- message was typed with, you will get it back.
expectMaybe' :: Binary a => ConnectionProcess (Maybe a)
expectMaybe' = do
  expected <- expectEither
  case expected of
    Right x -> return (Just x)
    Left bs -> do
      mapM_ (flip queueUpFp bs) (bsFingerprint bs)
      return Nothing

-- | Block and wait for the next incoming (typed) message that can be
-- successfully decoded as a value of that type.  All non-decodable
-- messages are thrown away and skipped over.
--
-- This is polymorphic in its return type, so you should either use the
-- result later somehow or explicitly annotate the type so that GHC knows
-- what you want.
--
-- __Warning!__: Incoming non-decodable data is thrown away and lost
-- forever with 'expect'.  If you wish to use this monomorphic channel
-- together with a dynamically typed polymorphic communication channel,
-- consider using 'expect'' to maintain typed queues for non-decodable
-- messages.
expect :: Binary a => ConnectionProcess a
expect = do
  res <- expectMaybe
  case res of
    Just res' -> return res'
    Nothing   -> expect

-- | Block and wait for the next incoming (typed) message that can be
-- successfully decoded as a value of that type.
--
-- This is polymorphic in its return type, so you should either use the
-- result later somehow or explicitly annotate the type so that GHC knows
-- what you want.
--
-- All non-decodable messages that are actually 'Tagged' 'ByteString's
-- (from _tagged-binary_) will be queued up with its type, so you can use
-- 'expectTagged' later to retrieve it.
expect' :: Binary a => ConnectionProcess a
expect' = do
  res <- expectMaybe'
  case res of
    Just res' -> return res'
    Nothing   -> expect'

-- | Block and wait for the next valid UTF8-encoded Text string.  Non-valid
-- messages are thrown away and skipped over.
expectText :: ConnectionProcess Text
expectText = decodeUtf8 <$> expect

-- | Block and wait for the next valid UTF8-encoded Text string.  Non-valid
-- messages that are 'Tagged' 'ByteString's from _tagged-binary_ will be
-- queued up with their type, so you can use 'expectTagged' later to
-- retrieve it.
expectText' :: ConnectionProcess Text
expectText' = decodeUtf8 <$> expect'

-- | A dynamic, polymorphic communication channel.  Can decode and queue
-- 'Tagged' 'ByteString' messages (from _tagged-binary_).
--
-- If there are any messages of the desired type in the queue, returns it
-- immediately.  Otherwise, blocks and waits for the first tagged message
-- of the desired type.  Any incoming messages that are not the proper type
-- are either queued (to be accessed when you want it) or thrown away (if
-- not tagged).
--
-- This is polymorphic in its return type, so you should either use the
-- result later somehow or explicitly annotate the type so that GHC knows
-- what you want.
--
-- Only works if the server sends a tagged message using _tagged-binary_,
-- of course.
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

