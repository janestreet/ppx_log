ppx_log
=======

A ppx rewriter that defines extension nodes for logging: primarily useful
to avoid the allocation of sexps that will never be logged.

```ocaml
[%log.debug log "example log" (content : Content.t)]
[%log.global.debug "example log" (content : Content.t)]
```

How to add to your project
--------------------------

First, add `ppx_log` to your jbuild:


```sexp
(preprocess (pps (ppx_jane ... ppx_log)))
```


If you have an `import.ml` file, then add `include Ppx_log_async` to use ppx_log
with Async.Log. Otherwise, you can simply `open Ppx_log_async` in the files that you
want to log.

`ppx_log` can add source code position to each logged message,
configurable at a library level by the `-log-source-position` switch:

```sexp
(preprocess (pps (ppx_jane ... ppx_log -log-source-position)))
```

Finally `[%log.debug]` to your heart's content!

Tests and examples
------------------
Take a look at [the mdx file](test/test-ppx-log.mdx) for tested examples.
