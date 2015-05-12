ghcjs-websockets
================

*ghcjs-websockets* aims to provide a clean, idiomatic, efficient, low-level,
out-of-your-way, bare bones, concurrency-aware interface with minimal
abstractions over the [Javascript Websockets API][jsapi], inspired by common
Haskell idioms found in libraries like [io-streams][] and the server-side
[websockets][] library, targeting compilation to Javascript with `ghcjs`.

The interface abstracts websockets as simple IO/file handles, with additional
access to the natively "typed" (text vs binary) nature of the Javascript
Websockets API.  There are also convenience functions to directly decode
serialized data (serialized with [binary][]) sent through channels.

The library is mostly intended to be a low-level FFI library, with the hopes
that other, more advanced libraries maybe build on the low-level FFI bindings
in order to provide more advanced and powerful abstractions.  Most design
decisions were made with the intent of keeping things as simple as possible in
order for future libraries to abstract over it.

Most of the necessary functionality is in hopefully in
`JavaScript.WebSockets`; more of the low-level API is exposed in
`JavaScript.WebSockets.Internal` if you need it for library construction.

Documenation is [online on github pages][documentation].

[jsapi]: http://www.w3.org/TR/websockets/
[io-streams]: http://hackage.haskell.org/package/io-streams
[websockets]: http://hackage.haskell.org/package/websockets
[binary]: http://hackage.haskell.org/package/binary
[documentation]: http://mstksg.github.io/ghcjs-websockets/JavaScript-WebSockets.html

Usage
-----

```haskell
import Data.Text (unpack)

-- A simple echo client, echoing all incoming text data
main :: IO ()
main = withUrl "ws://my-server.com" $ \conn ->
    forever $ do
        t <- receiveText conn
        putStrLn (unpack t)
        sendText conn t
```

The above code will attempt to interpret all incoming data as UTF8-encoded
Text, and throw away data that does not.

`conn` is a `Connection`, which encapsulates a websocket channel.

You can also do the same thing to interpret all incoming data as any instance
of `Binary` --- say, `Int`s:

```haskell
-- A simple client waiting for connections and outputting the running sum
main :: IO ()
main = withUrl "ws://my-server.com" (runningSum 0)

runningSum :: Int -> Connection -> IO ()
runningSum n conn = do
    i <- receiveData conn
    print (n + i)
    runningSum (n + i) conn
```

`receiveData` will block until the `Connection` receives data that is
decodable as whatever type you expect, and will throw away all nondecodable
data (including `Text` data).

The `receive` function is provided as an over-indulgent layer of abstraction
where you can receive both `Text` and instances of `Binary` with the same
function using typeclass magic --- for the examples above, you could use
`receive` in place of both `receiveText` and `receiveData`.

`send` works the same way for `sendText` and `sendData`.

If you want to, you can access the incoming data directly using the
`SocketMsg` sum type, which exposes either a `Text` or a lazy `ByteString`:

```haskell
import Data.Text (unpack, append)
import qualified Data.ByteString.Base64.Lazy as B64

main :: IO ()
main = withUrl "ws://my-server.com" $ \conn ->
    forever $ do
        msg <- receiveMessage
        putStrLn $ case msg of
            SocketMsgText t ->
                unpack $ append "Received text: " t
            SocketMsgData d ->
                "Received data: " ++ show (B64.encode d)
```

You can talk to multiple connections by nesting `withUrl`:

```haskell
-- Act as a relay between two servers
main :: IO ()
main =  withUrl "ws://server-1.com" $ \conn1 ->
        withUrl "ws://server-2.com" $ \conn2 ->
            forever $ do
                msg <- receiveMessage conn1
                sendMessage conn2 msg
```

And also alternatively, you can manually open and close connections:

```haskell
-- Act as a relay between two servers
main :: IO ()
main = do
    conn1 <- openConnection "ws://server-1.com"
    conn2 <- openConnection "ws://server-2.com"
    forever $ do
        msg <- receiveMessage conn1
        sendMessage conn2 msg
    closeConnection conn2
    closeConnection conn1
```

`receiveMessage` and its varieties will all throw an exception if the
connection closes while they're waiting or if you attempt to receive on a
closed connection.  You can handle these with mechanisms from
`Control.Exception`, or you can use their "maybe"-family counterparts,
`receiveMessageMaybe`, etc., who will return results in `Just` on a success,
or return a `Nothing` if the connection is closed or if receiving on a closed
connection.

You can use also `connectionClosed :: Connection -> IO Bool` to check if the
given `Connection` object is closed (or `connectionCloseReason` to see *why*).

When closing connections, there might be some messages that were received by
the socket but never processed on the Haskell side with a `receive` method.
These will normally be deleted; however, you can use
`closeConnectionLeftovers` or `withUrlLeftovers` to grab a list of the raw
`SocketMsg`s remaining after closing.

### Copyright

Copyright (c) Justin Le 2015

