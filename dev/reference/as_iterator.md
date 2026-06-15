# Transform an object to an iterator

`as_iterator()` is a generic function that transforms its input to an
[iterator function](https://coro.r-lib.org/dev/reference/iterator.md).
The default implementation is as follows:

- Functions are returned as is.

- Other objects are assumed to be vectors with
  [`length()`](https://rdrr.io/r/base/length.html) and `[[` methods.

Methods must return functions that implement coro's [iterator
protocol](https://coro.r-lib.org/dev/reference/iterator.md).

`as_iterator()` is called by coro on the RHS of `in` in `for` loops.
This applies within
[generators](https://coro.r-lib.org/dev/reference/generator.md), [async
functions](https://coro.r-lib.org/dev/reference/async.md), and
[`loop()`](https://coro.r-lib.org/dev/reference/collect.md).

## Usage

``` r
as_iterator(x)

# Default S3 method
as_iterator(x)
```

## Arguments

- x:

  An object.

## Value

An iterable function.

## Examples

``` r
as_iterator(1:3)
#> function () 
#> {
#>     if (i == n) {
#>         return(exhausted())
#>     }
#>     i <<- i + 1L
#>     x[[i]]
#> }
#> <bytecode: 0x556542215f68>
#> <environment: 0x55654220eb00>

i <- as_iterator(1:3)
loop(for (x in i) print(x))
#> [1] 1
#> [1] 2
#> [1] 3
```
