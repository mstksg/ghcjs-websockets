ghcjs-websockets
================

*ghcjs-websockets* aims to provide a clean, idiomatic, efficient, low-level,
out-of-your-way, bare bones, concurrency-aware interface with minimal
abstractions over the [Javascript Websockets API][jsapi], inspired by common
Haskell idioms found in libraries like [io-stream][] and the server-side
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

[jsapi]: http://www.w3.org/TR/2011/WD-websockets-20110419/
[io-stream]: http://hackage.haskell.org/package/io-streams
[websockets]: http://hackage.haskell.org/package/websockets
[binary]: http://hackage.haskell.org/package/binary

Usage
-----

```haskell
import Data.Text (unpack)

-- A simple echo client, echoing all incoming text data
main :: IO ()
main = withUrl "ws://my-server.com" $ \conn ->
    forever $ do
        t <- receiveText_ conn
        putStrLn (unpack t)
        sendText_ conn t
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
    i <- receiveData_ conn
    print (n + i)
    runningSum (n + i) conn
```

`receiveData_` will block until the `Connection` receives data that is
decodable as whatever type you expect, and will throw away all nondecodable
data (including `Text` data).

The `receive_` function is provided as a disgustingly over-indulgent and
unnecessary layer of abstraction where you can receive both `Text` and
instances of `Binary` with the same function using typeclass magic --- for the
examples above, you could use `receive_` in place of both `receiveText_` and
`receiveData_`.

`send_` works the same way for `sendText_` and `sendData_`.

If you want to, you can access the incoming data directly using the
`SocketMsg` sum type, which exposes either a `Text` or a lazy `ByteString`:

```haskell
import Data.Text (unpack, append)
import qualified Data.ByteString.Base64.Lazy as B64

main :: IO ()
main = withUrl "ws://my-server.com" $ \conn ->
    forever $ do
        msg <- receiveMessage_
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
                msg <- receiveMessage_ conn1
                sendMessage_ conn2 msg
```

And also alternatively, you can manually open and close connections:

```haskell
-- Act as a relay between two servers
main :: IO ()
main = do
    conn1 <- openConnection "ws://server-1.com"
    conn2 <- openConnection "ws://server-2.com"
    forever $ do
        msg <- receiveMessage_ conn1
        sendMessage_ conn2 msg
    closeConnection conn2
    closeConnection conn1
```

If you're manually working with connections like that, then the "safe",
non-underscore versions of `receive_`, `send_` are available.

forall `X`, `receiveX` behaves exactly like `receiveX_`, except it returns
`Maybe a` instead of `a`, and returns `Nothing` if the connection is closed or
if it closes while it's blocking/waiting.  If you attempt to `receiveX_` on a
closed connection or if the connection closes while you are waiting, a
`ConnectionException` will be thrown.

forall `X`, `sendX` behaves exactly like `sendX_`, except it returns `Bool`
instead of `()`, where the `Bool` indicates if connection you are trying to
send is open or not.  Trying to send message through a closed connection
returns `False` (and nothing is ever sent) and trying on an open connection
returns `True`.  This is different, technically, then checking if the message
was sent at all, as other things might go wrong further down the line, or with
the FFI API.  Hopefully stronger guarantees will be implemented in due time.

You can use also `connectionClosed :: Connection -> IO Bool` to check if the
given `Connection` object is closed.

### Copyright

Copyright (c) Justin Le 2014

