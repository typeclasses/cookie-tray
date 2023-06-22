The `cookie-tray` package aims to make it easy to set cookies from your web
server, even if you are not intimately familiar with the details of the
`Set-Cookie` HTTP field.

One example of a higher-level abstraction provided by this library is the idea
of cookie deletion. The low-level specification gives no way to explicitly
instruct a client to unset a cookie. This library provides a `Delete` action
that does the right thing (sets the cookie with an expiration date in the past)
to achieve removal.

---

Currently the best way to see how the library is used is to look at the test
suite.

---

I currently consider the API to be "experimental" because I don't think its
chief goals of being convenient and instructive have been satisfied confidently
enough to consider the library "finished." I am open to accepting improvements
to make the library more convenient to use.

---

This package works nicely in conjunction with [wai] and [warp]. See
[mapResponseHeaders] in WAI for how to apply fields to the response header.

I am not sure how one would use this with [scotty], as you may be thwarted by
Scotty's use of `Text` instead of `ByteString` to represent HTTP fields.

  [scotty]: https://hackage.haskell.org/package/scotty
  [mapResponseHeaders]: https://hackage.haskell.org/package/wai-3.2.3/docs/Network-Wai.html#v:mapResponseHeaders
  [wai]: https://hackage.haskell.org/package/wai
  [warp]: https://hackage.haskell.org/package/warp

---

Reference documentation on cookies:

* [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie)
* [IETF specification](https://datatracker.ietf.org/doc/html/rfc6265)
