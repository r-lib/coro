# Create a generator function

`generator()` creates an generator factory. A generator is an [iterator
function](https://coro.r-lib.org/dev/reference/iterator.md) that can
pause its execution with
[`yield()`](https://coro.r-lib.org/dev/reference/yield.md) and resume
from where it left off. Because they manage state for you, generators
are the easiest way to create iterators. See
[`vignette("generator")`](https://coro.r-lib.org/dev/articles/generator.md).

The following rules apply:

- Yielded values do not terminate the generator. If you call the
  generator again, the execution resumes right after the yielding point.
  All local variables are preserved.

- Returned values terminate the generator. If called again after a
  [`return()`](https://rdrr.io/r/base/function.html), the generator
  keeps returning the
  [`exhausted()`](https://coro.r-lib.org/dev/reference/iterator.md)
  sentinel.

Generators are compatible with all features based on the iterator
protocol such as
[`loop()`](https://coro.r-lib.org/dev/reference/collect.md) and
[`collect()`](https://coro.r-lib.org/dev/reference/collect.md).

## Usage

``` r
generator(fn)

gen(expr)
```

## Arguments

- fn:

  A function template for generators. The function can
  [`yield()`](https://coro.r-lib.org/dev/reference/yield.md) values.
  Within a generator, `for` loops have
  [iterator](https://coro.r-lib.org/dev/reference/iterator.md) support.

- expr:

  A yielding expression.

## See also

[`yield()`](https://coro.r-lib.org/dev/reference/yield.md),
[`coro_debug()`](https://coro.r-lib.org/dev/reference/coro_debug.md) for
step-debugging.

## Examples

``` r
# A generator statement creates a generator factory. The
# following generator yields three times and then returns `"d"`.
# Only the yielded values are visible to the callers.
generate_abc <- generator(function() {
  yield("a")
  yield("b")
  yield("c")
  "d"
})

# Equivalently:
generate_abc <- generator(function() {
  for (x in c("a", "b", "c")) {
    yield(x)
  }
})

# The factory creates generator instances. They are iterators
# that you can call successively to obtain new values:
abc <- generate_abc()
abc()
#> [1] "a"
abc()
#> [1] "b"

# Once a generator has returned it keeps returning `exhausted()`.
# This signals to its caller that new values can no longer be
# produced. The generator is exhausted:
abc()
#> [1] "c"
abc()
#> .__exhausted__.

# You can only exhaust a generator once but you can always create
# new ones from a factory:
abc <- generate_abc()
abc()
#> [1] "a"


# As generators implement the coro iteration protocol, you can use
# coro tools like `loop()`. It makes it possible to loop over
# iterators with `for` expressions:
loop(for (x in abc) print(x))
#> [1] "b"
#> [1] "c"

# To gather values of an iterator in a list, use `collect()`. Pass
# the `n` argument to collect that number of elements from a
# generator:
abc <- generate_abc()
collect(abc, 1)
#> [[1]]
#> [1] "a"
#> 

# Or drain all remaining elements:
collect(abc)
#> list()


# coro provides a short syntax `gen()` for creating one-off
# generator _instances_. It is handy to adapt existing iterators:
numbers <- 1:10
odds <- gen(for (x in numbers) if (x %% 2 != 0) yield(x))
squares <- gen(for (x in odds) yield(x^2))
greetings <- gen(for (x in squares) yield(paste("Hey", x)))

collect(greetings)
#> [[1]]
#> [1] "Hey 1"
#> 
#> [[2]]
#> [1] "Hey 9"
#> 
#> [[3]]
#> [1] "Hey 25"
#> 
#> [[4]]
#> [1] "Hey 49"
#> 
#> [[5]]
#> [1] "Hey 81"
#> 


# Arguments passed to generator instances are returned from the
# `yield()` statement on reentry:
new_tally <- generator(function() {
  count <- 0
  while (TRUE) {
    i <- yield(count)
    count <- count + i
  }
})
tally <- new_tally()
tally(1)
#> [1] 0
tally(2)
#> [1] 2
tally(10)
#> [1] 12
```
