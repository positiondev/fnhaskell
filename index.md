fn lets you write web code that just looks like normal Haskell code.

* An application has some "context", which must contain a Request, but can contain other data as well, like database connection pools, etc. This context will be passed to each of your handlers, updated with the current HTTP Request.

* Routes are declared to capture parameters and/or segments of the url, and then routed to handler functions that have the appropriate number and type of arguments. These functions return IO (Maybe Response), where Nothing indicates to Fn that you want it to keep looking for matching routes.

* All handlers just use plain old IO, which means it is easy to call them from GHCi, forkIO, etc.

* All of this is a small wrapper around the WAI interface, so you have the flexilibility to do anything you need to do with HTTP.

The name comes from the fact that fn emphasizes functions (over monads), where all necessary data is passed via function arguments, and control flow is mediated by return values.

## Example app

Paste the following into a file, chmod and run! (Requires [stack](http://docs.haskellstack.org/en/stable/README/))

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-5.5 --install-ghc runghc --package fn --package warp
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.Wai (Response)
import Network.Wai.Handler.Warp (run)
import Web.Fn

data Ctxt = Ctxt { _req :: FnRequest }
instance RequestContext Ctxt where
  getRequest = _req
  setRequest c r = c { _req = r }

initializer :: IO Ctxt
initializer = return (Ctxt defaultFnRequest)

main :: IO ()
main = do ctxt <- initializer
          run 3000 $ toWAI ctxt site

site :: Ctxt -> IO Response
site ctxt = route ctxt [ end                        ==> indexH
                       , path "echo" // param "msg" ==> echoH
                       , path "echo" // segment     ==> echoH
                       ]
                  `fallthrough` notFoundText "Page not found."

indexH :: Ctxt -> IO (Maybe Response)
indexH _ = okText "Try visiting /echo?msg=hello or /echo/hello"

echoH :: Ctxt -> Text -> IO (Maybe Response)
echoH _ msg = okText $ "Echoing '" <> msg <> "'."
```
