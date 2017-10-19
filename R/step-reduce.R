
reduce_steps <- function(.x, .steps, .builder, .init) {
  .builder <- as_closure(.builder)

  # Seal the transformation chain by supplying a builder function
  # (step reducer) to the steps wrapper (transducer). The stack of
  # transformation steps should have been composed in reverse order
  # (as is the default in `compose()`). This way the builder is
  # supplied to the very last step in the chain of transformations,
  # which itself is supplied to the penultimate step and so on. Input
  # data will then flow from outermost steps to innermost ones up to
  # the builder step which decides how to handle this result.
  if (is_null(.steps)) {
    reducer <- .builder
  } else {
    reducer <- .steps(.builder)
  }
  stopifnot(is_closure(reducer))

  # A builder called without argument should return an init value,
  # typically its identity. If a `.steps` wrapper is supplied, this
  # causes transducers to call their wrapped steps without arguments
  # up until the builder step.
  if (missing(.init)) {
    identity <- reducer()
  } else {
    identity <- .init
  }

  # This reduction causes a loop over the data. If `.steps` was
  # supplied the data flows through all transducers wrapped in
  # `reducer`.
  result <- reduce(.x, reducer, .init = identity)

  # Calling without input triggers completion within all
  # transformation steps.
  reducer(result)
}


into <- function(to, from, steps = NULL, coercer = NULL) {
  stopifnot(is_vector(to))
  reduce_steps(from, steps, into_builder(to, coercer))
}

take <- function(.x, .n) {
  reduce_steps(.x, take_step(.n), into_builder(list_len(.n)))
}
take_lgl <- function(.x, .n) {
  reduce_steps(.x, take_step(.n), into_builder(lgl_len(.n)))
}
take_int <- function(.x, .n) {
  reduce_steps(.x, take_step(.n), into_builder(int_len(.n)))
}
take_dbl <- function(.x, .n) {
  reduce_steps(.x, take_step(.n), into_builder(dbl_len(.n)))
}
take_cpl <- function(.x, .n) {
  reduce_steps(.x, take_step(.n), into_builder(cpl_len(.n)))
}
take_chr <- function(.x, .n) {
  reduce_steps(.x, take_step(.n), into_builder(chr_len(.n)))
}
take_raw <- function(.x, .n) {
  reduce_steps(.x, take_step(.n), into_builder(raw_len(.n)))
}


# From purrr. The only change is that this reduce() function supports
# reduced objects for early termination of reducing.
reduce <- function(.x, .f, ..., .init) {
  reduce_impl(.x, .f, ..., .init = .init)
}

reduce_impl <- function(.x, .f, ..., .init, .left = TRUE) {
  result <- reduce_init(.x, .init, left = .left)
  idx <- reduce_index(.x, .init, left = .left)

  .f <- as_closure(.f)
  for (i in idx) {
    result <- .f(result, .x[[i]], ...)

    # Return early if we get a reduced result
    if (is_reduced(result)) {
      return(unbox_reduced(result))
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

new_reduced <- function(x) {
  set_class(list(x), "reduced")
}
is_reduced <- function(x) {
  inherits(x, "reduced")
}
ensure_reduced <- function(x) {
  if (is_reduced(x)) {
    x
  } else {
    new_reduced(x)
  }
}
unbox_reduced <- function(x) {
  x[[1]]
}
