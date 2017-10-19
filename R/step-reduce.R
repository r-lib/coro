
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
