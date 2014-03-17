ghcjs-websockets
================

*ghcjs-websockets* aims to provide a clean, idiomatic, efficient, low-level,
out-of-your-way, concurrency-aware interface with minimal abstractions over
the [Javascript Websockets API][jsapi], inspired on common Haskell idioms
found in libraries like [io-stream][] and the server-side [websockets][]
library, targeting compilation to Javascript with `ghcjs`.

The interface abtracts websockets as simple IO/file handles, but also allows
access to the natively "typed" (text vs binary) nature of the Javascript
Websockets API for the optional ability to treat a single websocket connection
as a dual, "parallel" channel --- `Text` and `ByteString`/Serializable (with
[binary][]) data.

The library is mostly intended to be a low-level FFI library, with the hopes
that other, more advanced libraries maybe build on the low-level FFI bindings
in order to provide more advanced and powerful abstractions.

[jsapi]: http://www.w3.org/TR/2011/WD-websockets-20110419/
[io-stream]: http://hackage.haskell.org/package/io-streams
[websockets]: http://hackage.haskell.org/package/websockets
[binary]: http://hackage.haskell.org/package/binary

Examples
--------

```haskell
import Data.Text (unpack)

-- A simple echo client, echoing all incoming text data
main :: IO ()
main = withUrl "ws://my-server.com" $ \conn ->
    forever $ do
        t <- receive conn
        putStrLn (unpack t)
        sendText conn t
```

The above code will attempt to interpret all incoming data as UTF8-encoded
Text, and throw away data that does not.

You can also do the same thing to interpret all incoming data as any instance
of `Binary` --- say, `Int`s:

```haskell
-- A simple client waiting for connections and outputting the running sum
main :: IO ()
main = withUrl "ws://my-server.com" (runningSum 0)

runningSum :: Int -> Connection -> IO ()
runningSum n conn = do
    i <- receive conn
    print (n + i)
    runningSum (n + i) conn
```

`receive` will block until the `Connection` receives data that is decodable as
an `Int`, throwing away all data otherwise.

However, in the native Javascript Websockets API, all data that comes in is
"typed" as either Text or Data (Binary).  Why not take advantage of this, and
treat them as two parallel streams?

```haskell
import Control.Concurrent.MVar   (newEmptyMVar, takeMVar)

main :: IO ()
main = do
  conn <- openConneciton "ws://my-server.com"
  block <- newEmptyMVar
  forkIO           $ runningSum 0 conn
  forkIO . forever $ echo conn


```




*ghcjs-websockets* aims to provide an clean, idiomatic, *concurrent* Haskell
interface abstracting over the Javascript Websockets API, targeting `ghcjs`
for receiving serialized tagged and untagged data.

The interface abstracts websockets as raw data streams and is designed to
allow multiple styles of usage; in particular, is adapted for
[io-stream][]-style usage and [distributed-process][]-style usage.  It is
designed to fit in unintrusively in larger frameworks, and adaptable for other
interfaces (like *pipes*).

This library provides both *tagged* and *untagged* communication channels,
using [tagged-binary][].

* *Untagged* channels will throw away incoming binary data of unexpected type.

* *Tagged* channels will queue up binary data of unexpected type to be
  accessed later when data of that type is requested.

Tagged channels mimic the behavior of [Cloud Haskell][] and
[distributed-process][], with their dynamic communication channels.  You
can use the same channel to send in polymorphic, typed data and deal with it
at the time you wish.

Some examples
-------------

* Some *io-stream*-style usage.

```haskell
-- Echoes input from the server.
main :: IO ()
main = do
    c <- openConnection "ws://server-url.com"
    forever $ do
        d <- withConn c expectText
        putStrLn d
        withConn c (sendText d)
    closeConnection c
```

* Commands involving connections can be sequenced with a monadic interface.

```haskell
-- Echoes input from the server.
main :: IO ()
main = withUrl "ws://server-url.com" . forever $ do
    d <- expectText
    liftIO $ putStrLn d
    sendText d
```

* Wait for incoming data only decodable as a desired type, and skip over
  undecodable data.

```haskell
-- Keep on printing all `Just` values received, and stop at the first
-- `Nothing`.
whileJust :: ConnectionProcess ()
whileJust = do
    d <- expect
    case d of
      Just d' -> do
          liftIO $ putStrLn d'
          whileJust
      Nothing ->
          return ()
```

* Typed dynamic communication channels with [tagged-binary][]; channels
  looking for a specific type skip over input of the wrong type, and channels
  looking for the other type can pick them up later or in parallel.

```haskell
-- Server emits `Int`s or `String`s randomly; launch two parallel threads
-- to catch the data as it comes in, one watching for `Int`s and one watching
-- for `String`s.  Note the `forkProcess` `ConnectionProcess` command.
main :: IO ()
main = withUrl "ws://server-url.com" $ do
    block <- liftIO newEmptyMVar
    forkProcess . forever $ do
        n <- expectTagged
        replicateM n . liftIO . putStrLn $ "got a number! " ++ show n
    forkProcess . forever $ do
        s <- expectTagged
        liftIO $ putStrLn s
    liftIO $ takeMVar block
```

There is still some functionality left to be desired; feel free to open a
ticket and send in suggestions or bugs, and all pull requests are welcomed!

### Copyright

Copyright (c) Justin Le 2014


[tagged-binary]: http://hackage.haskell.org/package/tagged-binary
[io-stream]: http://hackage.haskell.org/package/io-streams
[iostream]: http://hackage.haskell.org/package/io-streams
[Cloud Haskell]: http://www.haskell.org/haskellwiki/Cloud_Haskell
[distributed-process]: http://hackage.haskell.org/package/distributed-process

