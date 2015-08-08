0.3.0.5
-------
<https://github.com/mstksg/ghcjs-websockets/releases/tag/v0.3.0.5>

*   Fixing `withURL` blocking when there is a javascript error on connection.
    (Thanks to sztupi)

0.3.0.4
-------
<https://github.com/mstksg/ghcjs-websockets/releases/tag/v0.3.0.4>

*   Fixed bug on double mutex acquisition for `connectionClosed`.

0.3.0.3
-------
<https://github.com/mstksg/ghcjs-websockets/releases/tag/v0.3.0.3>

**DEPRECATED:** Please use `0.3.0.4`!

*   Added CPP an cabal file flags necessary to enable building on (normal)
    GHC, for hackage and usage with hybrid projects.

0.3.0.2
-------
<https://github.com/mstksg/ghcjs-websockets/releases/tag/v0.3.0.2>

**DEPRECATED:** Please use `0.3.0.4`!

*   Lowered bounds on *text* dependency.
*   Added `CHANGELOG.md` and `README.md` to extra source dependecy fields, to
    count them in the cabal package.

0.3.0.1
-------
<https://github.com/mstksg/ghcjs-websockets/releases/tag/v0.3.0.1>

**DEPRECATED:** Please use `0.3.0.4`!

*   Fixed the "other-modules" cabal file field to include non-exported but
    important modules.

0.3.0.0
-------
<https://github.com/mstksg/ghcjs-websockets/releases/tag/v0.3.0.0>

**DEPRECATED:** Please use `0.3.0.4`!

*   First official release.  API more or less stabilized.  Library is more or
    less stable, but there are still some extra aspects of the javascript
    websockets API to hook onto for more power/information, and some
    javascript errors to be handled on edge cases.

