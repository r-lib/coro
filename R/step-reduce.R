#' Reduce with a chain of transformation
#'
#' @noRd
#' @description
#'
#' `reduce_steps()` is like [purrr::reduce()] but supports chains of
#' transformation steps. These transformations are applied to each
#' element of `x` and are passed as `input` to the builder function
#' `.builder` as final step. All transformations are applied in
#' sequence for each input, so no intermediary output is
#' constructed. This makes transformation steps ideal to transform
#' data chunk by chunk, especially when the whole dataset doesn't fit
#' in memory.
#'
#' The inner mechanism of transformation steps is a little bit
#' abstract and it takes some time understand how they work and how to
#' write one. However most of the time you shouldn't have to use
#' `reduce_steps()` or write your own transformation step. You will
#' typically use existing steps provided in coro (like [iter_map()]
#' and [iter_discard()]) with user-friendly wrappers such as
#' `iter_adapt()`.
#'
#' `reduce_steps()` and thus all functions based on it support coro
#' [iterators][iterator] and [generators][generator].
#'
#' `reduce_steps()` is equivalent to `transduce` in Clojure. See their
#' [documentation](https://clojure.org/reference/transducers).
#'
#'
#' @section The builder function:
#'
#' The builder function, usually called reducer function, is a
#' function that takes an accumulated result (i.e. the result so far)
#' as first argument, a new input as second argument, and combines
#' them into a new result. It is in charge of building the final
#' output.
#'
#' In addition to handling a result and an input, the builder function
#' must also support missing arguments:
#'
#' * If the result so far (i.e. the first argument) is missing, the
#'   builder function should return its identity. For example the
#'   identity for a function that builds a list would be the empty
#'   list. This identity value is used as initial "result so far" when
#'   `.init` is not supplied.
#'
#'   A missing result is also a signal that the reduction process has
#'   begun. This is a good time to initialise the builder state if
#'   needed. For this reason, `reduce_steps()` calls the transfomation
#'   chain without arguments even if `.init` has been supplied. This
#'   ensures that initialisation was properly supplied.
#'
#' * If the new input (i.e. the second argument) is missing, this is a
#'   signal that the reduction process has successfully completed.
#'   The builder function can finalise its output if needed.
#'
#'   Note that this only applies to successful completion and is meant
#'   for flushing pending results into the final output. It is
#'   therefore not appropriate to use the completion step as an exit
#'   event, e.g. to close resources or connections. If an error occurs
#'   the builder will not be called with missing input.
#'
#'
#' @section Transformation steps:
#'
#' The transformation steps, also called transducers, are higher-order
#' functions that take a function, transform its behaviour, and return
#' another function of the same type. In this case transducers take a
#' reducer function (one that takes a `result` so far and a new
#' `input`) and return another reducer function. The transformation
#' typically does one of two things:
#'
#' * In most cases it transforms the values passed as `input` and pass
#'   the modified values to the original reducer.
#'
#' * A transducer can also decide to completely ignore the new input
#'   and return the result so far without modification.
#'
#' Since a transformation step returns a reducer, this means they are
#' composable. You can pass a transformed reducer to another
#' transformation function. This is how you chain together multiple
#' transformations. However this chain should always be started with a
#' proper builder function. The transducers return reducers but these
#' reducers are not builder functions per se. E.g. they are not in
#' charge of building the final result and in fact they should never
#' do anything to `result` other than pass it along or return it
#' literally (in case the new input is ignored). This is why a chain
#' of transformation *must* be started with a builder function that
#' will be called last and actually does the work of building the
#' result with the new inputs.
#'
#' Building a chain of transformation is therefore done backwards. You
#' start with the very last step in the chain and work your way up to
#' the first step, the one that takes fresh inputs. However working in
#' reverse order is made natural by the `compose()` function which
#' composes all of its arguments in reverse. `compose()` is the ideal
#' way of chaining transformations.
#'
#' Note that `reduce_steps()` applies the builder function to a chain
#' of transformation for you, this way `reduce_steps(x, s, b)` is
#' basically equivalent to `reduce(x, s(b))`.
#'
#'
#' @section Early termination:
#'
#' The `reduce()` function used internally in coro has support for
#' early termination. If the reducer returns a `reduced` box
#' (constructed with [done()]), remaining inputs in `x` are
#' ignored and `reduce_steps()` finishes the reduction right away.
#'
#' @param x A vector to reduce or an [iterator].
#' @param steps A chain of transformation steps to apply before
#'   calling the builder function. If `NULL`, the builder function is
#'   reduced without transformation.
#' @param builder A builder function that takes a "result so far" as
#'   first argument and a new input as second argument. It is called
#'   iteratively by `reduce_steps()` with each elements of `x` as new
#'   input.
#' @param init An initial `result` value. If not supplied, the
#'   builder function is called without argument to get an initial
#'   value.
#'
#' @seealso [steps], [along_builder()]
#' @examples
#' # Let's create a chain of transformation. Let's start with the very
#' # last step, the builder function.
#'
#' # `c()` is a perfectly valid builder function. At initialisation it
#' # returns its identity (NULL) when called without arguments:
#' c()
#'
#' # It builds an output incrementally by repeatedly joining a
#' # "result so far" and a new input:
#' x <- NULL
#' x <- c(x, 1)
#' x <- c(x, 2)
#' x <- c(x, 3)
#' x
#'
#' # And it is a no-op when called with only one argument for completion:
#' c(x)
#'
#' # `c()` thus meets all specifications for builder functions. When
#' # used for reduction, it joins the inputs and grows an output using
#' # R implicit coercion rules:
#' inputs <- as.list(1:10)
#' reduce_steps(inputs, .steps = NULL, .builder = base::c)
#'
#' # Reducing inputs with `c()` is a recursive way of implementing
#' # `do.call(inputs, base::c)`.
#'
#'
#' # Let's now modify `c()` with transformation steps. A
#' # transformation chain is constructed in reverse order, so it
#' # always starts with the builder function:
#' steps <- base::c
#'
#' # Maybe we want to add `10` to all inputs. `iter_map()` lazily
#' # returns a transformation step. This is a function that accepts a
#' # reducer and returns another one:
#' iter_map(`+`, 10)
#'
#' # Let's supply our builder function `c()` to that first
#' # transformation step:
#' steps <- iter_map(`+`, 10)(steps)
#'
#' # We get a transformed reducer function that is ready to use with
#' # `reduce()` or `reduce_steps()`:
#' steps
#'
#' # We don't have to stop with only one transformation. The
#' # transformed reducer function can be supplied to other
#' # transformation steps and so on! Let's discard any input smaller
#' # than 5 in the next step:
#' steps <- iter_discard(`<`, 5)(steps)
#'
#' # Remember that we are building the transformations in reverse
#' # order though, so the inputs will be first passed to `discard`,
#' # then to `map`, and finally to the builder function `c`:
#' reduce_steps(inputs, .steps = NULL, .builder = steps)
#'
#'
#' # Building the chain in reverse order is rather inconvenient and
#' # unintuitive. It is recommended to use `purrr::compose()` to build
#' # a transformation chain:
#' compose <- purrr::compose
#' steps <- compose(iter_discard(`<`, 5), iter_map(`+`, 10))
#'
#' # `compose()` returns a function whose input will be supplied to
#' # the last composed function.
#' reduce_steps(inputs, NULL, steps(base::c))
#'
#' # A chain of composed transformation steps is handy because it can
#' # easily be reused for different builder functions! It is for this
#' # reason that `reduce_steps()` takes a transformation chain and a
#' # builder function separately:
#' reduce_steps(inputs, steps, base::c)
#' reduce_steps(inputs, steps, along_builder(""))
#'
#'
#' # reduce_steps() supports iterators as well:
#' iter <- as_iterator(1:5)
#' iter()
#' reduce_steps(iter, iter_map(`+`, 10), along_builder(list()))
#'
#' # By extension, all functions based on reduce_steps() support
#' # iterators:
#' iter <- as_iterator(1:50)
#' take(iter, 5)
#' take_chr(iter, 5)
reduce_steps <- function(x, steps, builder, init) {
  builder <- as_closure(builder)

  # Seal the transformation chain by supplying a builder function
  # (step reducer) to the steps wrapper (transducer). The stack of
  # transformation steps should have been composed in reverse order
  # (as is the default in `compose()`). This way the builder is
  # supplied to the very last step in the chain of transformations,
  # which itself is supplied to the penultimate step and so on. Input
  # data will then flow from outermost steps to innermost ones up to
  # the builder step which decides how to handle this result.
  if (is_null(steps)) {
    reducer <- builder
  } else {
    reducer <- steps(builder)
  }
  stopifnot(is_closure(reducer))

  # A builder called without argument should return an init value,
  # typically its identity. If a `steps` wrapper is supplied, this
  # causes transducers to call their wrapped steps without arguments
  # up until the builder step.
  if (missing(init)) {
    identity <- reducer()
  } else {
    identity <- init
  }

  # This reduction causes a loop over the data. If `steps` was
  # supplied the data flows through all transducers wrapped in
  # `reducer`.
  result <- reduce(x, reducer, .init = identity)

  # Calling without input triggers completion within all
  # transformation steps.
  reducer(result)
}


into <- function(to, from, steps = NULL) {
  stopifnot(is_vector(to))
  reduce_steps(from, steps, along_builder(to))
}

# From purrr. The only change is that this reduce() function supports
# reduced objects for early termination of reducing.
reduce <- function(.x, .f, ..., .init) {
  reduce_impl(.x, .f, ..., .init = .init)
}

reduce_impl <- function(.x, .f, ..., .init, .left = TRUE) {
  if (is.object(.x)) {
    .x <- as_iterator(.x)
  }
  if (is_closure(.x)) {
    return(iter_reduce_impl(.x, .f, ..., .left = .left))
  }

  result <- reduce_init(.x, .init, left = .left)
  idx <- reduce_index(.x, .init, left = .left)

  .f <- as_closure(.f)
  for (i in idx) {
    result <- .f(result, .x[[i]], ...)

    # Return early if we get a reduced result
    if (is_done_box(result)) {
      return(unbox(result))
    }
  }

  result
}
reduce_init <- function(x, init, left = TRUE) {
  if (missing(init)) {
    if (is_empty(x)) {
      abort("`.x` is empty, and no `.init` supplied")
    } else if (left) {
      x[[1]]
    } else {
      x[[length(x)]]
    }
  } else {
    init
  }
}
reduce_index <- function(x, init, left = TRUE) {
  n <- length(x)

  if (missing(init)) {
    if (left) {
      seq2(2L, n)
    } else {
      rev(seq2(1L, n - 1L))
    }
  } else {
    if (left) {
      seq_len(n)
    } else {
      rev(seq_len(n))
    }
  }
}

iter_reduce_impl <- function(.x, .f, ..., .init, .left = TRUE) {
  if (!.left) {
    abort("Can't right-reduce with an iterator.")
  }

  # TODO: How do we close transducers?
  defer(iter_close(.x))

  .f <- as_function(.f)

  out <- NULL

  while (!is_exhausted(new <- .x())) {
    out <- .f(out, new, ...)

    # Return early if we get a reduced result
    if (is_done_box(out)) {
      return(unbox(out))
    }
  }

  out
}

on_load(async_reduce_steps %<~% async(function(x, steps, builder, init) {
  builder <- as_closure(builder)

  if (is_null(steps)) {
    reducer <- builder
  } else {
    reducer <- steps(builder)
  }
  stopifnot(is_closure(reducer))

  if (missing(init)) {
    identity <- reducer()
  } else {
    identity <- init
  }

  result <- await(async_reduce(x, reducer))

  reducer(result)
}))

on_load(async_reduce %<~% async(function(.x, .f, ...) {
  out <- NULL

  while (TRUE) {
    new <- await(.x())

    if (is_exhausted(new)) {
      break
    }

    out <- .f(out, new, ...)

    # Return early if we get a reduced result
    if (is_done_box(out)) {
      return(unbox(out))
    }
  }

  out
}))
