# Changelog

## coro (development version)

- Fixed performance issue related to JIT compilation of generator/async
  instances ([\#71](https://github.com/r-lib/coro/issues/71)). The JIT
  compiler is now able to properly cache the instances, making repeated
  instantiations fast, and preventing the JIT from kicking in on every
  call.

- Async functions and generators can now be R6 methods
  ([\#63](https://github.com/r-lib/coro/issues/63)).

- A memory leak that crept back in (see
  [\#36](https://github.com/r-lib/coro/issues/36)) was fixed with a more
  robust approach.

## coro 1.1.0

CRAN release: 2024-11-05

- Iterator functions are now allowed to have a `close` argument. If they
  do, they will be called with a `close = TRUE` value when iteration is
  terminated early. This gives the opportunity to clean up resources.

- Generators now run [`on.exit()`](https://rdrr.io/r/base/on.exit.html)
  expressions when they are closed.

- Iterators managed by
  [`coro::loop()`](https://coro.r-lib.org/dev/reference/collect.md) and
  by generator `for` loops are now cleaned up when terminated early,
  either because of an error or because of a
  [`break`](https://rdrr.io/r/base/Control.html)
  ([\#52](https://github.com/r-lib/coro/issues/52)).

- Implicit and explicit return values of generators are no longer
  yielded. This is consistent with Javascript and Python and simplifies
  certain idioms ([\#51](https://github.com/r-lib/coro/issues/51)).

- Generators and async functions assigned in namespaces no longer
  produce R CMD check notes about visible bindings
  ([\#40](https://github.com/r-lib/coro/issues/40)).

## coro 1.0.5

CRAN release: 2024-10-15

- Async functions created by
  [`coro::async()`](https://coro.r-lib.org/dev/reference/async.md) now
  return their
  [`promises::promise()`](https://rstudio.github.io/promises/reference/promise.html)
  invisibly ([\#46](https://github.com/r-lib/coro/issues/46),
  [@shikokuchuo](https://github.com/shikokuchuo)).

- Fixes for CRAN checks.

## coro 1.0.4

CRAN release: 2024-03-11

- Internal fix for R-devel.

## coro 1.0.3

CRAN release: 2022-07-19

- [`coro::as_iterator()`](https://coro.r-lib.org/dev/reference/as_iterator.md)
  method added for reticulate Python objects, enabling usage like:
  `coro::loop(for (elem in my_py_object) ...)`
  ([\#37](https://github.com/r-lib/coro/issues/37),
  [@t-kalinowski](https://github.com/t-kalinowski)).

## coro 1.0.2

CRAN release: 2021-12-03

- The exhaustion sentinel is now `as.symbol(".__exhausted__.")` instead
  of `as.symbol("exhausted")` to reduce the risk of collisions. It is
  also recommended to never store the exhaustion sentinel in an
  environment or list, and instead use it as a temporary value to
  further reduce the risk of collisions
  ([\#35](https://github.com/r-lib/coro/issues/35)).

- Fixed a leak that occurred via JIT caching
  ([\#36](https://github.com/r-lib/coro/issues/36)).

## coro 1.0.1

CRAN release: 2020-12-17

- [`collect()`](https://coro.r-lib.org/dev/reference/collect.md) now
  preserves lists and data frames
  ([\#32](https://github.com/r-lib/coro/issues/32)).

## coro 1.0.0

CRAN release: 2020-12-09

This is the first public version of coro.

- Python iterators created with the reticulate package can now be
  composed with coro generators. They can also be used with
  [`loop()`](https://coro.r-lib.org/dev/reference/collect.md) and
  [`collect()`](https://coro.r-lib.org/dev/reference/collect.md).

- [`iterate()`](https://rstudio.github.io/reticulate/reference/iterate.html)
  has been renamed to
  [`loop()`](https://coro.r-lib.org/dev/reference/collect.md) to avoid
  name clash and semantic conflict with
  [`reticulate::iterate()`](https://rstudio.github.io/reticulate/reference/iterate.html).

- [`collect()`](https://coro.r-lib.org/dev/reference/collect.md) calls
  [`as_iterator()`](https://coro.r-lib.org/dev/reference/as_iterator.md)
  on its input.

- [`as_iterator()`](https://coro.r-lib.org/dev/reference/as_iterator.md)
  is now a generic function
  ([\#28](https://github.com/r-lib/coro/issues/28)).

- Generators and async functions now support
  [`on.exit()`](https://rdrr.io/r/base/on.exit.html) expressions. They
  also support exit expressions installed with functions like
  [`withr::local_options()`](https://withr.r-lib.org/reference/with_options.html).
  This requires R \>= 3.5.

- Generator arguments are forced on reentry in the approriate context
  (e.g. in the [`tryCatch()`](https://rdrr.io/r/base/conditions.html)
  context if generator was yielded from a
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html)). This makes it
  possible to clean up cancelled generators (by jumping from the
  generator with a restart) or propagate errors in async functions.

- Generators and async functions now support yielding within
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html) expressions.

- Generators and async functions are now disabled when an unexpected
  exit occurs (error, interrupt, restart invokation, debugger exit,
  etc.). Reentering the generator is an error.

- Generators and async functions now support stepping with
  [`browser()`](https://rdrr.io/r/base/browser.html). Set
  `options(coro_debug = TRUE)` for browsing all functions created with
  coro. Use
  [`coro_debug()`](https://coro.r-lib.org/dev/reference/coro_debug.md)
  for browsing specific functions.

- [`generator()`](https://coro.r-lib.org/dev/reference/generator.md) now
  accepts functions of one argument. The first time a generator is
  called the argument is defined in the suspendable function. On
  subsequent invokations, the argument is returned from
  [`yield()`](https://coro.r-lib.org/dev/reference/yield.md).

- [`generator()`](https://coro.r-lib.org/dev/reference/generator.md) now
  creates generator factories. It takes a function template and returns
  a function that creates generator functions from this template. This
  is consistent with languages like Javascript and Python.

- The [`async()`](https://coro.r-lib.org/dev/reference/async.md)
  function operator creates functions for cooperative concurrency using
  the *later* and *promises* framework.

  [`async_generator()`](https://coro.r-lib.org/dev/reference/async_generator.md)
  creates iterable functions that are also awaitable.

- The iterator abstraction has been removed from coro. It is replaced
  with the following protocol:

  - An iterator is a function. You advance it and get a new value by
    calling it.
  - An iterator signals exhaustion by returning
    [`exhausted()`](https://coro.r-lib.org/dev/reference/iterator.md)
    (or equivalently, the `quote(exhausted)` symbol).

- Exhausted generators no longer throw when called again. Instead, they
  return the
  [`exhausted()`](https://coro.r-lib.org/dev/reference/iterator.md)
  sentinel ([\#6](https://github.com/r-lib/coro/issues/6)).

- Fixed top-level [`break`](https://rdrr.io/r/base/Control.html)
  statements in loops ([\#7](https://github.com/r-lib/coro/issues/7)).

## coro 0.0.1

Initial release
