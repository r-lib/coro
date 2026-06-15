# Construct an async generator

An async generator constructs iterable functions that are also
awaitables. They support both the
[`yield()`](https://coro.r-lib.org/dev/reference/yield.md) and
[`await()`](https://coro.r-lib.org/dev/reference/async.md) syntax. An
async iterator can be looped within async functions and iterators using
`await_each()` on the input of a `for` loop.

The iteration protocol is derived from the one described in
[`iterator`](https://coro.r-lib.org/dev/reference/iterator.md). An async
iterator always returns a promise. When the iterator is exhausted, it
returns a resolved promise to the exhaustion sentinel.

## Usage

``` r
async_generator(fn)

await_each(x)
```

## Arguments

- fn:

  An anonymous function describing an async generator within which
  [`await()`](https://coro.r-lib.org/dev/reference/async.md) calls are
  allowed.

- x:

  An awaitable value, i.e. a
  [promise](https://rstudio.github.io/promises/reference/promise.html).

## Value

A generator factory. Generators constructed with this factory always
return
[`promises::promise()`](https://rstudio.github.io/promises/reference/promise.html).

## See also

[`async()`](https://coro.r-lib.org/dev/reference/async.md) for creating
awaitable functions;
[`async_collect()`](https://coro.r-lib.org/dev/reference/async_collect.md)
for collecting the values of an async iterator;
[`coro_debug()`](https://coro.r-lib.org/dev/reference/coro_debug.md) for
step-debugging.

## Examples

``` r
# Creates awaitable functions that transform their inputs into a stream
generate_stream <- async_generator(function(x) for (elt in x) yield(elt))

# Maps a function to a stream
async_map <- async_generator(function(.i, .fn, ...) {
  for (elt in await_each(.i)) {
    yield(.fn(elt, ...))
  }
})

# Example usage:
if (interactive()) {
  library(magrittr)
  generate_stream(1:3) %>% async_map(`*`, 2) %>% async_collect()
}
```
