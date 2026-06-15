# Yield a value from a generator

The `yield()` statement suspends
[`generator()`](https://coro.r-lib.org/dev/reference/generator.md)
functions. It works like
[`return()`](https://rdrr.io/r/base/function.html) except that the
function continues execution at the yielding point when it is called
again.

`yield()` can be called within loops and if-else branches but for
technical reasons it can't be used anywhere in R code:

- `yield()` cannot be called as part of a function argument. Code such
  as `list(yield())` is illegal.

- `yield()` does not cross function boundaries. You can't use it a
  lambda function passed to
  [`lapply()`](https://rdrr.io/r/base/lapply.html) for instance.

## Usage

``` r
yield(x)
```

## Arguments

- x:

  A value to yield.

## See also

[`generator()`](https://coro.r-lib.org/dev/reference/generator.md) for
examples.
