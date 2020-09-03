
# flowery 0.0.1.9000

* The iterator abstraction has been removed from flowery. It is
  replaced with the following protocol:

  - An iterator is a function. You advance it and get a new value by
    calling it.
  - An iterator signals exhaustion by returning `NULL`.

  The `NULL` sentinel synergises well with the R control flow
  constructs like `while` which return `NULL` when they are done.

  Consequently you need to be a bit careful when `yield()`ing
  `NULL`. Previously, yielded `NULL` values would be escaped without
  closing the iterator. Now, this will signal exhaustion. If you need
  to return arbitrary objects, box them in a size 1 list, e.g. with
  `rlang::new_box()`, and instruct your callers to unbox the returned
  values, e.g. with `rlang::unbox()`.

* Exhausted generators no longer throw when called again. Instead,
  they return `NULL` (#6).

* Fixed top-level `break` statements in loops (#7).


# flowery 0.0.1

Initial release
