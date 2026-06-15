# Generators

## Iterators

Generators are a simple way of creating **iterator functions**,
i.e. functions that you can call to return a new value. The iteration
protocol is described in
[`?iterator`](https://coro.r-lib.org/dev/reference/iterator.md). Here is
a simple iterator that iterates over the elements of `1:3`:

``` r

library(coro)

iterator <- as_iterator(1:3)

# Call the iterator to retrieve new values
iterator()
#> [1] 1

iterator()
#> [1] 2
```

Once the iterator is exhausted, it returns a sentinel value that signals
to its caller that there are no more values available:

``` r

# This is the last value
iterator()
#> [1] 3

# This is the exhaustion sentinel
iterator()
#> .__exhausted__.
```

In R we normally don’t use this sort of iteration to work with vectors.
Instead, we use the idiomatic techniques of vectorised programming.
Iterator functions are useful for very specific tasks:

- Iterating over **chunks** of data when the whole data doesn’t fit in
  memory.

- Generating **sequences** when you don’t know in advance how many
  elements you will need. These sequences may be complex and even
  infinite.

The iterator protocol is designed to be free of dependency. However, the
easiest way to create an iterator is by using the generator factories
provided in this package.

## Generators

Generators create functions that can **yield**, i.e. suspend themselves.
When a generator reaches a `yield(value)` statement it returns the value
as if you called `return(value)`. However, calling the generator again
resumes the function right where it left off. Because they preserve
their state between invokations, generators are ideal for creating
iterator functions.

``` r

generate_abc <- generator(function() {
  for (x in letters[1:3]) {
    yield(x)
  }
})
```

[`generator()`](https://coro.r-lib.org/dev/reference/generator.md)
creates an **iterator factory**. This is a function that returns fresh
iterator functions:

``` r

# Create the iterator
abc <- generate_abc()

# Use the iterator by invoking it
abc()
#> [1] "a"

abc()
#> [1] "b"
```

Once the last loop in a generator has finished iterating (here there is
only one), it returns the exhaustion sentinel:

``` r

# Last value
abc()
#> [1] "c"

# Exhaustion sentinel
abc()
#> .__exhausted__.

abc()
#> .__exhausted__.
```

You can also create infinite iterators that can’t be exhausted:

``` r

generate_natural_numbers <- generator(function(from = 1L) {
  x <- from
  repeat {
    yield(x)
    x <- x + 1L
  }
})

natural_numbers <- generate_natural_numbers(from = 10L)

# The iterator generates new numbers forever
natural_numbers()
#> [1] 10

natural_numbers()
#> [1] 11
```

## Iterating

Iterating manually over an iterator function is a bit tricky because you
have to watch out for the exhaustion sentinel:

``` r

abc <- generate_abc()

while (!is_exhausted(x <- abc())) {
  print(x)
}
#> [1] "a"
#> [1] "b"
#> [1] "c"
```

A simpler way is to iterate with a `for` loop using the
[`loop()`](https://coro.r-lib.org/dev/reference/collect.md) helper.
Within [`loop()`](https://coro.r-lib.org/dev/reference/collect.md),
`for` understands the iterator protocol:

``` r

abc <- generate_abc()

loop(for (x in abc) {
  print(x)
})
#> [1] "a"
#> [1] "b"
#> [1] "c"
```

You can also collect all remaning values of an iterator in a list with
[`collect()`](https://coro.r-lib.org/dev/reference/collect.md):

``` r

abc <- generate_abc()

collect(abc)
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "b"
#> 
#> [[3]]
#> [1] "c"
```

Beware that trying to exhaust an infinite iterator is a programming
error. This causes an infinite loop that never returns, forcing the user
to interrupt R with `ctrl-c`. Make sure that you iterate over an
infinite iterator only a finite amount of time:

``` r

for (x in 1:3) {
  print(natural_numbers())
}
#> [1] 12
#> [1] 13
#> [1] 14

collect(natural_numbers, n = 3)
#> [[1]]
#> [1] 15
#> 
#> [[2]]
#> [1] 16
#> 
#> [[3]]
#> [1] 17
```

## Adapting generators

A generator factory can take another iterator as argument to modify its
values. This pattern is called **adapting**:

``` r

library(magrittr)

adapt_toupper <- generator(function(i) {
  for (x in i) {
    yield(toupper(x))
  }
})

ABC <- generate_abc() %>% adapt_toupper()

ABC()
#> [1] "A"

ABC()
#> [1] "B"
```

Once the modified iterator is exhausted, the adaptor automatically
closes as well:

``` r

ABC()
#> [1] "C"

ABC()
#> .__exhausted__.
```

As a user, you might not want to create an iterator factory for a
one-off adaptor. In this case you can use
[`gen()`](https://coro.r-lib.org/dev/reference/generator.md) instead of
[`generator()`](https://coro.r-lib.org/dev/reference/generator.md). This
enables a more pythonic style of working with iterators:

``` r

abc <- generate_abc()
ABC <- gen(for (x in abc) yield(toupper(x)))

collect(ABC)
#> [[1]]
#> [1] "A"
#> 
#> [[2]]
#> [1] "B"
#> 
#> [[3]]
#> [1] "C"
```

Or you can use the general purpose adaptor `adapt_map()`. It maps a
function over each value of an iterator:

``` r

adapt_map <- generator(function(.i, .fn, ...) {
  for (x in .i) {
    yield(.fn(x, ...))
  }
})

ABC <- generate_abc() %>% adapt_map(toupper)

ABC()
#> [1] "A"
```
