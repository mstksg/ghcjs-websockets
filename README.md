ghcjs-websockets
================

*ghcjs-websockets* aims to provide an clean, idiomatic Haskell interface
abstracting over the Javascript Websockets API, targeting `ghcjs` for
receiving serialized tagged and untagged data.

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
-- for `String`s.
main :: IO ()
main = do
   c <- openTaggedConnection "ws://server-url.com"
   t1 <- forkIO . withConn c . forever $ do
       n <- expectTagged
       replicateM n . liftIO . putStrLn $ "got a number! " ++ show n
   t2 <- forkIO . withConn c . forever $ do
       s <- expectTagged
       liftIO $ putStrN s
   await t1
   await t2
   closeConnection c
```

There is still some functionality left to be desired; feel free to open a
ticket and send in suggestions or bugs, and all pull requests are welcomed!

### Copyright

Copyright (c) Justin Le 2014


[tagged-binary]: http://hackage.haskell.org/package/tagged-binary
[io-stream]: http://hackage.haskell.org/package/io-streams
[Cloud Haskell]: http://www.haskell.org/haskellwiki/Cloud_Haskell
[distributed-process]: http://hackage.haskell.org/package/distributed-process

