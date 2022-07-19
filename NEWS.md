# coro 1.0.3

* `coro::as_iterator()` method added for reticulate Python objects,
  enabling usage like: `coro::loop(for (elem in my_py_object) ...)`
  (#37, @t-kalinowski).


# coro 1.0.2

* The exhaustion sentinel is now `as.symbol(".__exhausted__.")`
  instead of `as.symbol("exhausted")` to reduce the risk of
  collisions. It is also recommended to never store the exhaustion
  sentinel in an environment or list, and instead use it as a
  temporary value to further reduce the risk of collisions (#35).

* Fixed a leak that occurred via JIT caching (#36).


# coro 1.0.1

* `collect()` now preserves lists and data frames (#32).


# coro 1.0.0

This is the first public version of coro.

* Python iterators created with the reticulate package can now be
  composed with coro generators. They can also be used with `loop()`
  and `collect()`.

* `iterate()` has been renamed to `loop()` to avoid name clash and
  semantic conflict with `reticulate::iterate()`.

* `collect()` calls `as_iterator()` on its input.

* `as_iterator()` is now a generic function (#28).

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
  `browser()`. Set `options(coro_debug = TRUE)` for browsing all
  functions created with coro. Use `coro_debug()` for browsing
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

* The iterator abstraction has been removed from coro. It is
  replaced with the following protocol:

  - An iterator is a function. You advance it and get a new value by
    calling it.
  - An iterator signals exhaustion by returning `exhausted()` (or
    equivalently, the `quote(exhausted)` symbol).

* Exhausted generators no longer throw when called again. Instead,
  they return the `exhausted()` sentinel (#6).

* Fixed top-level `break` statements in loops (#7).


# coro 0.0.1

Initial release
