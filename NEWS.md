
# flowery 0.0.1.9000

* `generator()` now accepts functions of one argument. The first time
  a generator is called the argument is defined in the suspendable
  function. On subsequent invokations, the argument is returned from
  `yield()`.

* `generator()` now takes anonymous functions.

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
