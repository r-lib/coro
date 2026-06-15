# Iterate over iterator functions

`loop()` and `collect()` are helpers for iterating over [iterator
functions](https://coro.r-lib.org/dev/reference/iterator.md) such as
[generators](https://coro.r-lib.org/dev/reference/generator.md).

- `loop()` takes a `for` loop expression in which the collection can be
  an iterator function.

- `collect()` loops over the iterator and collects the values in a list.

## Usage

``` r
collect(x, n = NULL)

loop(loop)
```

## Arguments

- x:

  An iterator function.

- n:

  The number of elements to collect. If `x` is an infinite sequence, `n`
  must be supplied to prevent an infinite loop.

- loop:

  A `for` loop expression.

## Value

`collect()` returns a list of values; `loop()` returns the
[`exhausted()`](https://coro.r-lib.org/dev/reference/iterator.md)
sentinel, invisibly.

## See also

[`async_collect()`](https://coro.r-lib.org/dev/reference/async_collect.md)
for async generators.

## Examples

``` r
generate_abc <- generator(function() for (x in letters[1:3]) yield(x))
abc <- generate_abc()

# Collect 1 element:
collect(abc, n = 1)
#> [[1]]
#> [1] "a"
#> 

# Collect all remaining elements:
collect(abc)
#> list()

# With exhausted iterators collect() returns an empty list:
collect(abc)
#> list()


# With loop() you can use `for` loops with iterators:
abc <- generate_abc()
loop(for (x in abc) print(x))
#> [1] "a"
#> [1] "b"
#> [1] "c"
```
