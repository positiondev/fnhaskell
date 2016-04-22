fn lets you write web code that just looks like normal Haskell code.

* An application has some "context", which must contain a Request, but can contain other data as well, like database connection pools, etc. This context will be passed to each of your handlers, updated with the current HTTP Request.

* Routes are declared to capture parameters and/or segments of the url, and then routed to handler functions that have the appropriate number and type of arguments. These functions return IO (Maybe Response), where Nothing indicates to Fn that you want it to keep looking for matching routes.

* All handlers just use plain old IO, which means it is easy to call them from GHCi, forkIO, etc.

* All of this is a small wrapper around the WAI interface, so you have the flexilibility to do anything you need to do with HTTP.

The name comes from the fact that fn emphasizes functions (over monads), where all necessary data is passed via function arguments, and control flow is mediated by return values.

POSITION DEV
