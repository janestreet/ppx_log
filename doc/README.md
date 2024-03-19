# ppx_log

ppx_log is the recommended way of writing logs to `Async.Log`. To use it, simply `open
Async`, then you can write statements like:

```ocaml
[%log log "example log" (content : Content.t)]
[%log.global "example log" (content : Content.t)]
```

Some benefits of ppx_log:
* Avoids computing the message if it wouldn't be logged 
* Preserves structure instead of flattening to a sexp for outputs that can make use of it

You can also hook it up to your own logging libraries.

Features and examples
------------------
Take a look at [the mdx file](examples.mdx) for tested examples.
