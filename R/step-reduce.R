
reduce_steps <- function(.x, .step, .init_step, .init) {
  init_step <- as_closure(.init_step)

  # Seal the transducing chain by supplying the initial step (reducer)
  # to the step joiner (transducer) chain. This chain should have been
  # built in reverse order (as is the default in `compose()`) so that
  # the initial step is supplied to the very first step in the chain.
  if (is_null(.step)) {
    reducer <- .init_step
  } else {
    reducer <- .step(init_step)
  }
  stopifnot(is_closure(reducer))

  if (missing(.init)) {
    # An initial step called without argument should return an init
    # value, typically its identity. If a `.step` chain is supplied,
    # this causes transducers to call their wrapped steps without
    # arguments up until the initial step.
    identity <- reducer()
  } else {
    identity <- .init
  }

  # This reduction causes a loop over the data that iteratively calls
  # all transducers wrapped in `reducer`.
  result <- reduce(.x, reducer, .init = identity)

  # An initial step called without `input` should finalise the output
  # if needed. If a `.step` chain is supplied, this causes all
  # transducers to call their wrapped steps with a single `result`
  # argument up until the initial step.
  reducer(result)
}


into <- function(to, from, step = NULL, coercer = NULL) {
  stopifnot(is_vector(to))
  reduce_steps(from, step, into_init_step(to, coercer))
}

take <- function(.x, .n) {
  reduce_steps(.x, take_step(.n), into_init_step(list_len(.n)))
}
take_lgl <- function(.x, .n) {
  reduce_steps(.x, take_step(.n), into_init_step(lgl_len(.n)))
}
take_int <- function(.x, .n) {
  reduce_steps(.x, take_step(.n), into_init_step(int_len(.n)))
}
take_dbl <- function(.x, .n) {
  reduce_steps(.x, take_step(.n), into_init_step(dbl_len(.n)))
}
take_cpl <- function(.x, .n) {
  reduce_steps(.x, take_step(.n), into_init_step(cpl_len(.n)))
}
take_chr <- function(.x, .n) {
  reduce_steps(.x, take_step(.n), into_init_step(chr_len(.n)))
}
take_raw <- function(.x, .n) {
  reduce_steps(.x, take_step(.n), into_init_step(raw_len(.n)))
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
