# Async operations

**\[experimental\]**

Customisation point for the *async* package or any concurrency framework
that defines a "then" operation. Assign the result of `async_ops()` to
the `.__coro_async_ops__.` symbol in your namespace.

## Usage

``` r
async_ops(package, then, as_promise)
```

## Arguments

- package:

  The package name of the framework as a string.
  [`async()`](https://coro.r-lib.org/dev/reference/async.md) and
  [`async_generator()`](https://coro.r-lib.org/dev/reference/async_generator.md)
  check that the package is installed at runtime.

- then:

  A function of two arguments. The first argument is a promise object
  (as created by `as_promise`). The second argument is a callback
  function that must be called once the promise object is resolved.

- as_promise:

  A function of one argument. It should be a no-op when passed a promise
  object, and otherwise wrap the value in a resolved promise.
