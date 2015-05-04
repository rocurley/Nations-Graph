% A wreq tutorial
% Learn how to write web clients. We start easy, then ramp up the power.

# Installation

To use the `wreq` package, simply use `cabal`, the standard Haskell
package management command.

~~~~
cabal update
cabal install -j --disable-tests wreq
~~~~

Depending on how many prerequisites you already have installed, and
what your Cabal configuration looks like, the build may take a few
minutes: a few seconds for `wreq`, and the rest for its dependencies.


# Interactive usage

We'll run our examples interactively via the `ghci` shell.

~~~~
$ ghci
~~~~

To start using `wreq`, we import the
[`Network.Wreq`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html)
module.

~~~~ {.haskell}
ghci> import Network.Wreq
ghci> r <- get "http://httpbin.org/get"
ghci> :type r
r :: Response ByteString
~~~~

The variable `r` above is the
[`Response`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#t:Response)
from the server.


## Working with string-like types

Complex Haskell libraries and applications have to deal fluently with
Haskell's three main string types: `String` ("legacy"), `Text`, and
`ByteString` (mostly used for binary data, sometimes ASCII).

To write string literals without having to always provide a conversion
function, we use the `OverloadedStrings` language extension.

Throughout the rest of this tutorial, we'll assume that you have
enabled `OverloadedStrings` in `ghci`:

~~~~ {.haskell}
ghci> :set -XOverloadedStrings
~~~~

If you're using `wreq` from a Haskell source file, put a pragma at the
top of your file:

~~~~ {.haskell}
{-# LANGUAGE OverloadedStrings #-}
~~~~


# A quick lens backgrounder

The `wreq` package makes heavy use of Edward Kmett's
[`lens`](https://lens.github.io/) package to provide a clean,
consistent API.

~~~~ {.haskell}
ghci> import Control.Lens
~~~~

While `lens` has a vast surface area, the portion that you must
understand in order to productively use `wreq` is tiny.

A lens provides a way to focus on a portion of a Haskell value. For
example, the `Response` type has a
[`responseStatus`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:responseStatus)
lens, which focuses on the status information returned by the server.

~~~~ {.haskell}
ghci> r ^. responseStatus
Status {statusCode = 200, statusMessage = "OK"}
~~~~

The
[`^.`](http://hackage.haskell.org/package/lens/docs/Control-Lens-Getter.html#v:-94-.)
operator takes a value as its first argument, a lens as its second,
and returns the portion of the value focused on by the lens.

We compose lenses using function composition, which allows us to
easily focus on part of a deeply nested structure.

~~~~ {.haskell}
ghci> r ^. responseStatus . statusCode
200
~~~~

We'll have more to say about lenses as this tutorial proceeds.


# Changing default behaviours

While
[`get`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:get)
 is convenient and easy to use, there's a lot more power
available to us.

For example, if we want to add parameters to the query string of a
URL, we will use the
[`getWith`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:getWith)
function.  The `*With` family of functions all accept an
[`Options`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#t:Options)
parameter that allow changes from the library's default behaviours.

~~~~ {.haskell}
ghci> import Data.Aeson.Lens (_String, key)
ghci> let opts = defaults & param "foo" .~ ["bar", "quux"]
ghci> r <- getWith opts "http://httpbin.org/get"
ghci> r ^. responseBody . key "url" . _String
"http://httpbin.org/get?foo=bar&foo=quux"
~~~~

(We'll talk more about `key` and `_String` below.)

The default parameters for all queries is represented by the variable
[`defaults`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:defaults).
(In fact, `get` is defined simply as `getWith defaults`.)

Here's where we get to learn a little more about lenses.

In addition to *getting* a value from a nested structure, we can also
*set* (edit) a value within a nested structure, which makes an
identical copy of the structure except for the portion we want to
modify.

The `&` operator is just function application with its operands
reversed, so the function is on the right and its parameter is on the
left.

~~~~ {.haskell}
parameter & functionToApply
~~~~

The
[`.~`](http://hackage.haskell.org/package/lens/docs/Control-Lens-Setter.html#v:.-126-)
 operator turns a lens into a setter function, with the lens
on the left and the new value on the right.

~~~~ {.haskell}
param "foo" .~ ["bar", "quux"]
~~~~

<a id="param">The
[`param`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:param)
lens</a> focuses on the values associated with the given key in the
query string.

~~~~ {.haskell}
param :: Text -> Lens' Options [Text]
~~~~

The reason we allow for a list of values instead of just a single
value is simply that this is completely legitimate. For instance, in
our example above we generate the query string `foo=bar&foo=quux`.

If you use non-ASCII characters in a `param` key or value, they will
be encoded as UTF-8 before being URL-encoded, so that they can be
safely transmitted over the wire.


# Accessing the body of a response

The
[`responseBody`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:responseBody)
lens gives us access to the body of a response.

~~~~ {.haskell}
ghci> r <- get "http://httpbin.org/get"
ghci> r ^. responseBody
"{\n  \"headers\": {\n    \"Accept-Encoding\": \"gzip"{-...-}
~~~~

The response body is a raw lazy
[`ByteString`](http://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Lazy.html#t:ByteString).


## JSON responses

We can use the
[`asJSON`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:asJSON)
function to convert a response body to a Haskell value that implements
the
[`FromJSON`](http://hackage.haskell.org/package/aeson/docs/Data-Aeson-Types.html#t:FromJSON)
class.

~~~~ {.haskell}
ghci> import Data.Map as Map
ghci> import Data.Aeson (Value)
ghci> type Resp = Response (Map String Value)
ghci> r <- asJSON =<< get "http://httpbin.org/get" :: IO Resp
ghci> Map.size (r ^. responseBody)
4
~~~~

<div class="alert alert-info">
In this example, we have to tell `ghci` exactly what target type we
are expecting. In a real Haskell program, the correct return type will
usually be inferred automatically, making an explicit type signature
unnecessary in most cases.
</div>

If the response is not `application/json`, or we try to convert to an
incompatible Haskell type, a
[`JSONError`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#t:JSONError)
exception will be thrown.

~~~~ {.haskell}
ghci> type Resp = Response [Int]
ghci> r <- asJSON =<< get "http://httpbin.org/get" :: IO Resp
*** Exception: JSONError "when expecting a [a], encountered Object instead"
~~~~


## Convenient JSON traversal

The `lens` package provides some extremely useful functions for
traversing JSON structures without having to either build a
corresponding Haskell type or traverse a `Value` by hand.

The first of these is
[`key`](http://hackage.haskell.org/package/lens/docs/Data-Aeson-Lens.html#v:key),
which traverses to the named key in a JSON object.

~~~~ {.haskell}
ghci> import Data.Aeson.Lens (key)
ghci> r <- get "http://httpbin.org/get"
ghci> r ^? responseBody . key "url"
Just (String "http://httpbin.org/get")
~~~~

<div class="alert alert-info">
Notice our use of the
[`^?`](http://hackage.haskell.org/package/lens-4.1.2/docs/Control-Lens-Fold.html#v:-94--63-)
operator here. This is like `^.`, but it allows for the possibility
that an access might fail---and of course there may not be a key named
`"url"` in our object.
</div>

That said, our result above has the type `Maybe Value`, so it's quite
annoying to work with. This is where the `_String` lens comes in.

~~~~ {.haskell}
ghci> import Data.Aeson.Lens (_String, key)
ghci> r <- get "http://httpbin.org/get"
ghci> r ^. responseBody . key "url" . _String
"http://httpbin.org/get"
~~~~

If the key exists, and is a `Value` with a `String` constructor,
`_String` gives us back a regular `Text` value with all the wrappers
removed; otherwise it gives an empty value. Notice what happens as we
switch between `^?` and `^.` in these examples.

~~~~ {.haskell}
ghci> r ^. responseBody . key "fnord" . _String
""
ghci> r ^? responseBody . key "fnord" . _String
Nothing
ghci> r ^? responseBody . key "url" . _String
Just "http://httpbin.org/get"
~~~~


# Working with headers

To add headers to a request, we use the
[`header`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:header)
lens.

~~~~ {.haskell}
ghci> let opts = defaults & header "Accept" .~ ["application/json"]
ghci> getWith opts "http://httpbin.org/get"
~~~~

As with the [`param`](#param) lens, if we provide more than one value to go
with a single key, this will expand to several headers.

~~~~ {.haskell}
header :: HeaderName -> Lens' Options [ByteString]
~~~~

When we want to inspect the headers of a response, we use the
[`responseHeader`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:responseHeader)
lens.

~~~~ {.haskell}
ghci> r <- get "http://httpbin.org/get"
ghci> r ^. responseHeader "content-type"
"application/json"
~~~~

<div class="alert alert-info">
Header names are case insensitive.
</div>

If a header is not present in a response, then `^.` will give an empty
string, while `^?` will give `Nothing`.

~~~~ {.haskell}
ghci> r ^. responseHeader "X-Nonesuch"
""
ghci> r ^? responseHeader "X-Nonesuch"
Nothing
~~~~


# Uploading data via POST

We use the [`post`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:post)
 and
 [`postWith`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:postWith)
 functions to issue POST requests.

~~~~ {.haskell}
ghci> r <- post "http://httpbin.org/post" ["num" := 3, "str" := "wat"]
ghci> r ^? responseBody . key "form"
Just (Object fromList [("num",String "3"),("str",String "wat")])
~~~~

The [httpbin.org](http://httpbin.org/) server conveniently echoes our
request headers back at us, so we can see what kind of body we POSTed.

~~~~ {.haskell}
ghci> r ^. responseBody . key "headers" . key "Content-Type" . _String
"application/x-www-form-urlencoded"
~~~~

The
[`:=`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v::-61-)
operator is the constructor for the
[`FormParam`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#t:FormParam)
type, which `wreq` uses as a key/value pair to generate an
`application/x-www-form-urlencoded` form body to upload.

A class named
[`FormValue`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#t:FormValue)
determines how the operand on the right-hand side of `:=` is encoded,
with sensible default behaviours for strings and numbers.

The slightly more modern way to upload POST data is via a
`multipart/form-data` payload, for which `wreq` provides the
[`Part`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#t:Part)
type.

~~~~ {.haskell}
ghci> r <- post "http://httpbin.org/post" [partText "button" "o hai"]
ghci> r ^. responseBody . key "headers" . key "Content-Type" . _String
"multipart/form-data; boundary=----WebKitFormBoundaryJsEZfuj89uj"
~~~~

The first argument to these `part*` functions is the label of the
`<input>` element in the form being uploaded.

Let's inspect httpbin.org's response to see what we uploaded.  When we
think there could be more than one value associated with a lens, we
use the
[`^..`](http://hackage.haskell.org/package/lens-4.1.2/docs/Control-Lens-Fold.html#v:-94-..)
operator, which returns a list.

~~~~ {.haskell}
ghci> r ^.. responseBody . key "form"
[Object fromList [("button",String "o hai")]]
~~~~


## Uploading file contents

To upload a file as part of a `multipart/form-data` POST, we use
[`partFile`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#t:partFile),
or if the file is large enough that we want to stream its contents,
[`partFileSource`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#t:partFileSource).

~~~~ {.haskell}
ghci> import Data.Aeson.Lens (members)
ghci> r <- post "http://httpbin.org/post" (partFile "file" "hello.hs")
ghci> r ^.. responseBody . key "files" . members . _String
["main = putStrLn \"hello\"\n"]
~~~~

Both `partFile` and `partFileSource` will set the filename of a part
to whatever name they are given, and guess its content-type based on
the file name extension.  Here's an example of how we can upload a
file without revealing its name.

~~~~ {.haskell}
ghci> partFile "label" "foo.hs" & partFileName .~ Nothing
Part "label" Nothing (Just "text/plain") <m (RequestBody m)>
~~~~


# Cookies

To see how easily we can work with cookies, let's ask the
ever-valuable httpbin.org to set a cookie in a response.

~~~~ {.haskell}
ghci> r <- get "http://httpbin.org/cookies/set?foo=bar"
ghci> r ^. responseCookie "foo" . cookieValue
"bar"
~~~~

To make cookies even easier to deal with, we'll want to
[use the `Session` API](#session), but we'll come back to that later.


# Authentication

The `wreq` library supports both basic authentication and OAuth2
bearer authentication.

<div class="alert alert-danger">
**Note:** the security of these mechanisms is _absolutely dependent on
your use of TLS_, as the credentials can easily be stolen and reused
if transmitted unencrypted.
</div>

If we try to access a service that requires authentication, `wreq`
will throw a
[`HttpException`](http://hackage.haskell.org/package/http-client/docs/Network-HTTP-Client.html#t:HttpException).

~~~~ {.haskell}
ghci> r <- get "http://httpbin.org/basic-auth/user/pass"
*** Exception: StatusCodeException (Status {statusCode = 401, {-...-}
~~~~

If we then supply a username and password, our request will succeed.
(Notice that we follow our own advice: we switch to `https` for our
retry.)

~~~~ {.haskell}
ghci> let opts = defaults & auth ?~ basicAuth "user" "pass"
ghci> r <- getWith opts "https://httpbin.org/basic-auth/user/pass"
ghci> r ^. responseBody
"{\n  \"authenticated\": true,\n  \"user\": \"user\"\n}"
~~~~

<div class="alert alert-info">
We use the
[`?~`](http://hackage.haskell.org/package/lens/docs/Control-Lens-Setter.html#v:-63--126-)
 operator to turn an [`Auth`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#t:Auth)
into a `Maybe Auth` here, to make the type of value on the right hand
side compatible with the
[`auth`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:auth)
lens.
</div>

For OAuth2 bearer authentication, `wreq` supports two flavours:
[`oauth2Bearer`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:oauth2Bearer)
is the standard bearer token, while
[`oauth2Token`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:oauth2Token)
is GitHub's variant.  These tokens are equivalent in value to a
username and password.

## Amazon Web Services (AWS)
To authenticate to Amazon Web Services (AWS), we use
[`awsAuth`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:awsAuth). In
this example, we set the `Accept` header to request JSON, as opposed
to XML output from AWS.

~~~~ {.haskell}
ghci> let opts = defaults & auth ?~ awsAuth AWSv4 "key" "secret"
                          & header "Accept" .~ ["application/json"]
ghci> r <- getWith opts "https://sqs.us-east-1.amazonaws.com/?Action=ListQueues"
ghci> r ^. responseBody
"{\"ListQueuesResponse\":{\"ListQueuesResult\":{\"queueUrls\": ... }"
~~~~

## Runscope support for Amazon Web Services (AWS) requests
To send requests to AWS through the [Runscope Inc.](https://www.runscope.com)
Traffic Inspector, convert the AWS service URL to a Runscope Bucket URL
using the "URL Helper" section in the Runscope dashboard (as you
would for other HTTP endpoints). Then invoke the AWS service as
before.  For example, if your Runscope bucket key is
`7kh11example`, call AWS like so:

~~~~ {.haskell}
ghci> let opts = defaults & auth ?~ awsAuth AWSv4 "key" "secret"
                          & header "Accept" .~ ["application/json"]
ghci> r <- getWith opts "https://sqs-us--east--1-amazonaws-com-7kh11example.runscope.net/?Action=ListQueues"
ghci> r ^. responseBody
"{\"ListQueuesResponse\":{\"ListQueuesResult\":{\"queueUrls\": ... }"
~~~~

If you enabled "Require Authentication Token" in the "Bucket Settings"
of your Runscope dashboard, set the `Runscope-Bucket-Auth` header like so:

~~~~ {.haskell}
ghci> let opts = defaults & auth ?~ awsAuth AWSv4 "key" "secret"
                          & header "Accept" .~ ["application/json"]
                          & header "Runscope-Bucket-Auth" .~ ["1example-1111-4yyyy-zzzz-xxxxxxxx"]
ghci> r <- getWith opts "https://sqs-us--east--1-amazonaws-com-7kh11example.runscope.net/?Action=ListQueues"
ghci> r ^. responseBody
"{\"ListQueuesResponse\":{\"ListQueuesResult\":{\"queueUrls\": ... }"
~~~~


# Error handling

Most of the time when an error occurs or a request fails, `wreq` will
throw a `HttpException`.

~~~~ {.haskell}
h> r <- get "http://httpbin.org/wibblesticks"
*** Exception: StatusCodeException (Status {statusCode = 404, {-...-}
~~~~

Here's a simple example of how we can respond to one kind of error: a
`get`-like function that retries with authentication if an
unauthenticated request fails.

~~~~ {.haskell}
import Control.Exception as E
import Control.Lens
import Network.HTTP.Client
import Network.Wreq

getAuth url myauth = get url `E.catch` handler
  where
    handler e@(StatusCodeException s _ _)
      | s ^. statusCode == 401 = getWith authopts authurl
      | otherwise              = throwIO e
      where authopts = defaults & auth .~ myauth
            -- switch to TLS when we use auth
            authurl = "https" ++ dropWhile (/=':') url
~~~~

(A "real world" version would remember which URLs required
authentication during a session, to avoid the need for an
unauthenticated failure followed by an authenticated success if we
visit the same endpoint repeatedly.)


# Handling multiple HTTP requests

<a id="session">For non-trivial applications</a>, we'll always want to
use a
[`Session`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq-Session.html#t:Session)
to efficiently and correctly handle multiple requests.

The `Session` API provides two important features:

* When we issue multiple HTTP requests to the same server, a `Session`
  will reuse TCP and TLS connections for us.  (The simpler API we've
  discussed so far does not do this.)  This greatly improves
  efficiency.

* A `Session` transparently manages HTTP cookies.  (We can manage them
  by hand, but it's awkward and verbose, so we won't cover it in this
  tutorial.)

Here's a complete example.

~~~~ {.haskell}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Network.Wreq
import qualified Network.Wreq.Session as S

main :: IO ()
main = S.withSession $ \sess -> do
  -- First request: tell the server to set a cookie
  S.get sess "http://httpbin.org/cookies/set?name=hi"

  -- Second request: the cookie should still be set afterwards.
  r <- S.post sess "http://httpbin.org/post" ["a" := (3 :: Int)]
  print $ r ^. responseCookie "name" . cookieValue
~~~~

The key differences from the basic API are as follows.

* We import the
  [`Network.Wreq.Session`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq-Session.html)
  module qualified, and we'll identify its functions by prefixing them
  with "`S.`".

* To create a `Session`, we use `S.withSession`. It calls our code
  with `sess`, the `Session` value we'll use.

* Instead of `get` and `post`, we call the `Session`-specific
  versions, [`S.get`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq-Session.html#v:get) and [`S.post`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq-Session.html#v:post), and pass `sess` to each of them.
