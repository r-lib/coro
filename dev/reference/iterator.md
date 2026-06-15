# Iterator protocol

An **iterator** is a function that implements the following protocol:

- Calling the function advances the iterator. The new value is returned.

- When the iterator is exhausted and there are no more elements to
  return, `coro::exhausted()` or (equivalently)
  `as.symbol(".__exhausted__.")` is returned. This signals exhaustion to
  the caller.

- Once an iterator has signalled exhaustion, all subsequent invokations
  must consistently return `coro::exhausted()` or
  `as.symbol(".__exhausted__.")`.

- The iterator function may have a `close` argument taking boolean
  values. When passed a `TRUE` value, it indicates early termination and
  the iterator is given the opportunity to clean up resources.

  Cleanup must only be performed once, even if the iterator is called
  multiple times with `close = TRUE`.

  An iterator is allowed to not have any `close` argument. Iterator
  drivers must check for the presence of the argument. If not present,
  the iterator can be dropped without cleanup.

  An iterator passed `close = TRUE` must return `coro::exhausted()` and
  once closed, an iterator must return `coro::exhausted()` when called
  again.

    iterator <- as_iterator(1:3)

    # Calling the iterator advances it
    iterator()
    #> [1] 1
    iterator()
    #> [1] 2

    # This is the last value
    iterator()
    #> [1] 3

    # Subsequent invokations return the exhaustion sentinel
    iterator()
    #> .__exhausted__.

Because iteration is defined by a protocol, creating iterators is free
of dependency. However, it is often simpler to create iterators with
[generators](https://coro.r-lib.org/dev/reference/generator.md), see
[`vignette("generator")`](https://coro.r-lib.org/dev/articles/generator.md).
To loop over an iterator, it is simpler to use the
[`loop()`](https://coro.r-lib.org/dev/reference/collect.md) and
[`collect()`](https://coro.r-lib.org/dev/reference/collect.md) helpers
provided in this package.

## Usage

``` r
exhausted()

is_exhausted(x)
```

## Arguments

- x:

  An object.

## Properties

Iterators are **stateful**. Advancing the iterator creates a persistent
effect in the R session. Also iterators are **one-way**. Once you have
advanced an iterator, there is no going back and once it is exhausted,
it stays exhausted.

Iterators are not necessarily finite. They can also represent infinite
sequences, in which case trying to exhaust them is a programming error
that causes an infinite loop.

## The exhausted sentinel

Termination of iteration is signalled via a sentinel value,
`as.symbol(".__exhausted__.")`. Alternative designs include:

- A condition as in python.

- A rich value containing a termination flag as in Javascript.

The sentinel design is a simple and efficient solution but it has a
downside. If you are iterating over a collection of elements that
inadvertently contains the sentinel value, the iteration will be
terminated early. To avoid such mix-ups, the sentinel should only be
used as a temporary value. It should be created from scratch by a
function like `coro::exhausted()` and never stored in a container or
namespace.
