# wreq: a Haskell web client library [![Build Status](https://travis-ci.org/bos/wreq.svg)](https://travis-ci.org/bos/wreq)

`wreq` is a library that makes HTTP client programming in Haskell
easy.

# Features

* Simple but powerful `lens`-based API

* Over 100 tests, and built on reliable libraries like [`http-client`](http://hackage.haskell.org/package/http-client/)
  and [`lens`](https://lens.github.io/)

* Session handling includes connection keep-alive and pooling, and
  cookie persistence

* Automatic decompression

* Powerful multipart form and file upload handling

* Support for JSON requests and responses, including navigation of
  schema-less responses

* Basic and OAuth2 bearer authentication

* Amazon Web Services (AWS) request signing (Version 4)

* AWS signing supports sending requests through the
  [Runscope Inc.](https://www.runscope.com) Traffic Inspector

# Tutorials

See [the tutorials](http://www.serpentine.com/wreq/) for a quick-start.

# Is it done?

No!  See [`TODO.md`](TODO.md) for a rather long list of ideas.
