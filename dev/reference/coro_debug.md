# Debug a generator or async function

- Call `coro_debug()` on a
  [`generator()`](https://coro.r-lib.org/dev/reference/generator.md),
  [`async()`](https://coro.r-lib.org/dev/reference/async.md), or
  [`async_generator()`](https://coro.r-lib.org/dev/reference/async_generator.md)
  function to enable step-debugging.

- Alternatively, set `options(coro_debug = TRUE)` for step-debugging
  through all functions created with coro.

## Usage

``` r
coro_debug(fn, value = TRUE)
```

## Arguments

- fn:

  A generator factory or an async function.

- value:

  Whether to debug the function.
