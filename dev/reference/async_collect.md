# Collect elements of an asynchronous iterator

`async_collect()` takes an asynchronous iterator, i.e. an iterable
function that is also awaitable. `async_collect()` returns an awaitable
that eventually resolves to a list containing the values returned by the
iterator. The values are collected until exhaustion unless `n` is
supplied. The collection is grown geometrically for performance.

## Usage

``` r
async_collect(x, n = NULL)
```

## Arguments

- x:

  An iterator function.

- n:

  The number of elements to collect. If `x` is an infinite sequence, `n`
  must be supplied to prevent an infinite loop.

## Examples

``` r

# Emulate an async stream by yielding promises that resolve to the
# elements of the input vector
generate_stream <- async_generator(function(x) for (elt in x) yield(elt))

# You can await `async_collect()` in an async function. Once the
# list of values is resolved, the async function resumes.
async(function() {
  stream <- generate_stream(1:3)
  values <- await(async_collect(stream))
  values
})
#> <async>
#> function () 
#> {
#>     stream <- generate_stream(1:3)
#>     values <- await(async_collect(stream))
#>     values
#> }
#> <environment: 0x5565464922c8>
```
