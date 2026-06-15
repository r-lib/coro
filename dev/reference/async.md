# Make an async function

`async()` functions are building blocks for cooperative concurrency.

- They are *concurrent* because they are jointly managed by a scheduler
  in charge of running them.

- They are *cooperative* because they decide on their own when they can
  no longer make quick progress and need to **await** some result. This
  is done with the `await()` keyword which suspends the async function
  and gives control back to the scheduler. The scheduler waits until the
  next async operation is ready to make progress.

The async framework used by `async()` functions is implemented in the
[later](https://github.com/r-lib/later/) and
[promises](https://rstudio.github.io/promises/) packages:

- You can chain async functions created with coro to promises.

- You can await promises. You can also await futures created with the
  [future](https://github.com/HenrikBengtsson/future) package because
  they are coercible to promises.

## Usage

``` r
async(fn)

await(x)
```

## Arguments

- fn:

  An anonymous function within which `await()` calls are allowed.

- x:

  An awaitable value, i.e. a
  [promise](https://rstudio.github.io/promises/reference/promise.html).

## Value

A function that returns a
[`promises::promise()`](https://rstudio.github.io/promises/reference/promise.html)
invisibly.

## See also

[`async_generator()`](https://coro.r-lib.org/dev/reference/async_generator.md)
and
[`await_each()`](https://coro.r-lib.org/dev/reference/async_generator.md);
[`coro_debug()`](https://coro.r-lib.org/dev/reference/coro_debug.md) for
step-debugging.

## Examples

``` r
# This async function counts down from `n`, sleeping for 2 seconds
# at each iteration:
async_count_down <- async(function(n) {
  while (n > 0) {
    cat("Down", n, "\n")
    await(async_sleep(2))
    n <- n - 1
  }
})

# This async function counts up until `stop`, sleeping for 0.5
# seconds at each iteration:
async_count_up <- async(function(stop) {
  n <- 1
  while (n <= stop) {
    cat("Up", n, "\n")
    await(async_sleep(0.5))
    n <- n + 1
  }
})

# You can run these functions concurrently using `promise_all()`
if (interactive()) {
  promises::promise_all(async_count_down(5), async_count_up(5))
}
```
