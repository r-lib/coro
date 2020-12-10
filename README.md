
<!-- README.md is generated from README.Rmd. Please edit that file -->

# coro

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/coro)](https://cran.r-project.org/package=coro)
[![R build
status](https://github.com/r-lib/coro/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/coro/actions)
<!-- badges: end -->

## Overview

coro implements **coroutines** for R, i.e. functions that can be
suspended and resumed later on. There are two kinds:

  - **Async** functions, which make it straightforward to program
    concurrently
  - **Generators** for iterating over complex sequences

Supported features:

  - Suspending within loops and if/else branches
  - Suspending within `tryCatch()`
  - `on.exit()` expressions and stack-based cleanup such as provided by
    `local_` functions in the [withr](https://github.com/r-lib/withr/)
    package
  - Step-debugging and `browser()` within coroutines

Compatibility with:

  - Python iterators from the
    [reticulate](https://rstudio.github.io/reticulate/) package
  - Async operations from the
    [promises](https://github.com/rstudio/promises/) package
  - Parallel computations from the
    [future](https://github.com/HenrikBengtsson/future) package

Attach the package to follow the examples:

``` r
library(coro)
```

### Async/await functions

Concurrent programming is made straightforward by async-await functions.
Whenever you are waiting for a result that may take a while (downloading
a file, computing a value in an external process), use `await()`. The
argument to `await()` must return a promise from the
[promises](https://github.com/rstudio/promises/) package.

Concurrent code based on promises can quickly become hard to write and
follow. In the following artificial example, we wait for a download to
complete, then decide to launch a computation in an external process
depending on a property of the downloaded data. We also handle some
errors specifically.

``` r
my_async <- function() {
  async_download() %>%
    then(function(data) {
      if (ncol(data) > 10) {
        then(future::future(fib(30)), function(fib) {
          data / fib
        })
      } else {
        data
      }
    }, onRejected = function(err) {
      if (inherits(err, "download_error")) {
        NULL
      } else {
        stop(err)
      }
    })
}
```

Rewriting this function with async/await greatly simplifies the code:

``` r
my_async <- async(function() {
  data <- tryCatch(
    await(async_download()),
    download_error = function(err) NULL
  )

  if (is.null(data)) {
    return(NULL)
  }

  if (ncol(data) > 10) {
    fib <- await(future::future(fib(30)))
    data <- data /fib
  }

  data
})
```

### Generators

Generators are based on a simple iteration protocol:

  - Iterators are functions.
  - They can be advanced by calling the function. The new value is
    returned.
  - An exhausted iterator returns the sentinel symbol `exhausted`.

The `generator()` function creates a generator factory which returns
generator instances:

``` r
# Create a generator factory
generate_abc <- generator(function() {
  for (x in letters[1:3]) {
    yield(x)
  }
})

# Create a generator instance
abc <- generate_abc()
```

A generator instance is an iterator function which yields values:

``` r
abc
#> <generator/instance>
#> function() {
#>   for (x in letters[1:3]) {
#>     yield(x)
#>   }
#> }

abc()
#> [1] "a"
```

Collect all remaining values from an iterator with `collect()`:

``` r
collect(abc)
#> [[1]]
#> [1] "b"
#> 
#> [[2]]
#> [1] "c"
```

Iterate over an iterator with `loop()`:

``` r
loop(for (x in generate_abc()) {
  print(toupper(x))
})
#> [1] "A"
#> [1] "B"
#> [1] "C"
```

See `vignette("generator")` for more information.

### Compatibility with the reticulate package

Python iterators imported with the
[reticulate](https://rstudio.github.io/reticulate/) package are
compatible with `loop()` and `collect()`:

``` r
suppressMessages(library(reticulate))

py_run_string("
def first_n(n):
    num = 1
    while num <= n:
        yield num
        num += 1
")

loop(for (x in py$first_n(3)) {
  print(x * 2)
})
#> [1] 2
#> [1] 4
#> [1] 6
```

They can also be composed with coro generators:

``` r
times <- generator(function(it, n) for (x in it) yield(x * n))

composed <- times(py$first_n(3), 10)

collect(composed)
#> [[1]]
#> [1] 10
#> 
#> [[2]]
#> [1] 20
#> 
#> [[3]]
#> [1] 30
```

## Limitations

`yield()` and `await()` can be used in loops, if/else branches,
`tryCatch()` expressions, or any combinations of these. However they
can’t be used as function arguments. These will cause errors:

``` r
generator(function() {
  list(yield("foo"))
})

async(function() {
  list(await(foo()))
})
```

Fortunately it is easy to rewrite the code to work around this
limitation:

``` r
generator(function() {
  x <- yield("foo")
  list(x)
})

async(function() {
  x <- await(foo())
  list(x)
})
```

## How does it work

Coroutines are an [abstraction for state
machines](https://eli.thegreenplace.net/2009/08/29/co-routines-as-an-alternative-to-state-machines)
in languages that support them. Conversely, you can implement coroutines
by rewriting the code source provided by the user as a state machine.
Pass `internals = TRUE` to the print methods of coroutines to reveal the
state machine that is running under the hood:

``` r
print(generate_abc, internals = TRUE)
#> <generator>
#> function() {
#>   for (x in letters[1:3]) {
#>     yield(x)
#>   }
#> }
#> State machine:
#> {
#>     if (exhausted) {
#>         return(invisible(exhausted()))
#>     }
#>     repeat switch(state[[1L]], `1` = {
#>         iterators[[2L]] <- as_iterator(user(letters[1:3]))
#>         state[[1L]] <- 2L
#>         state[[2L]] <- 1L
#>     }, `2` = {
#>         repeat switch(state[[2L]], `1` = {
#>             if ({
#>                 iterator <- iterators[[2L]]
#>                 if (is_exhausted(elt <- iterator())) {
#>                   FALSE
#>                 } else {
#>                   user_env[["x"]] <- elt
#>                   TRUE
#>                 }
#>             }) {
#>                 state[[2L]] <- 2L
#>             } else {
#>                 break
#>             }
#>         }, `2` = {
#>             user({
#>                 x
#>             })
#>             state[[2L]] <- 3L
#>             suspend()
#>             return(last_value())
#>         }, `3` = {
#>             .last_value <- if (missing(arg)) NULL else arg
#>             state[[2L]] <- 1L
#>         })
#>         iterators[[2L]] <- NULL
#>         length(state) <- 1L
#>         break
#>     })
#>     exhausted <- TRUE
#>     invisible(exhausted())
#> }
```

Despite this transformation of source code, `browser()` and
step-debugging still work as you would expect. This is because coro
keeps track of the source references from the original code.

## Acknowledgements

  - The [regenerator](https://facebook.github.io/regenerator/)
    Javascript package which uses a similar transformation to implement
    generators and async functions in older versions of Javascript.

  - Gabor Csardi for many interesting discussions about concurrency and
    the design of coro.

## Installation

Install the development version from github with:

``` r
# install.packages("devtools")
devtools::install_github("r-lib/coro", build_vignettes = TRUE)
```
