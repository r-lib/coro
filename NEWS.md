
# flowery 0.0.1.9000

* Generators and async functions now support `on.exit()`
  expressions. They also support exit expressions installed with
  functions like `withr::local_options()`. This requires R >= 3.5.

* Generator arguments are forced on reentry in the approriate context
  (e.g. in the `tryCatch()` context if generator was yielded from a
  `tryCatch()`). This makes it possible to clean up cancelled
  generators (by jumping from the generator with a restart) or
  propagate errors in async functions.

* Generators and async functions now support yielding within
  `tryCatch()` expressions.

* Generators and async functions are now disabled when an unexpected
  exit occurs (error, interrupt, restart invokation, debugger exit,
  etc.). Reentering the generator is an error.

* Generators and async functions now support stepping with
  `browser()`. Set `options(flowery_debug = TRUE)` for browsing all
  functions created with flowery. Use `flowery_debug()` for browsing
  specific functions.

* `generator()` now accepts functions of one argument. The first time
  a generator is called the argument is defined in the suspendable
  function. On subsequent invokations, the argument is returned from
  `yield()`.

* `generator()` now creates generator factories. It takes a function
  template and returns a function that creates generator functions
  from this template. This is consistent with languages like
  Javascript and Python.

* The `async()` function operator creates functions for cooperative
  concurrency using the _later_ and _promises_ framework.

  `async_generator()` creates iterable functions that are also
  awaitable.

* The iterator abstraction has been removed from flowery. It is
  replaced with the following protocol:

  - An iterator is a function. You advance it and get a new value by
    calling it.
  - An iterator signals exhaustion by returning `NULL`.
  - Yielding `NULL` is an error.

  The `NULL` sentinel synergises well with the R control flow
  constructs like `while` which return `NULL` when they are done.
  This design is experimental and may be reviewed later on.

* Exhausted generators no longer throw when called again. Instead,
  they return `NULL` (#6).

* Fixed top-level `break` statements in loops (#7).


# flowery 0.0.1

Initial release
