
iter <- function(body, env = caller_env()) {
  body <- enexpr(body)
  fn <- new_function(body, env = env)
  new_iterator(fn)
}

new_iterator <- function(fn) {
  stopifnot(is_closure(fn))

  # Flag so methods can check that they have an iterator
  `_flowery_iterator` <- TRUE

  done <- FALSE
  last <- NULL

  iter <- function() {
    if (done) {
      abort("Iterator is done")
    }

    out <- withVisible(fn())
    last <<- out$value

    if (is_null(last)) {
      done <<- TRUE
    } else if (is_box(last, "null_box")) {
      last <<- NULL
    }

    if (out$visible) {
      last
    } else {
      invisible(last)
    }
  }

  set_attrs(iter, class = "iterator")
}

is_iterator <- function(x) {
  inherits(x, "iterator")
}

deref <- function(x) {
  stopifnot(is_iterator(x))
  env_get(iter_env(x), "last")
}
advance <- function(x) {
  stopifnot(is_iterator(x))
  !(is_null(x()) && is_done(x))
}
is_done <- function(x) {
  stopifnot(is_iterator(x))
  env_get(iter_env(x), "done")
}

iter_env <- function(iter) {
  env <- get_env(iter)
  if (!env_has(env, "_flowery_iterator")) {
    abort("Expected an iterator")
  }
  env
}

print.iterator <- function(x, ...) {
  cat("<iterator>\n")
  fn <- env_get(iter_env(x), "fn")
  print(fn)

  invisible(x)
}

null_box <- function() {
  box(NULL, "null_box")
}

# Requires length() and `[[` methods
as_iterator <- function(x) {
  if (is_iterator(x)) {
    return(x)
  }
  if (is_closure(x)) {
    return(new_iterator(x))
  }

  n <- length(x)
  i <- 0L

  iter <- function() {
    if (i == n) {
      return(NULL)
    }

    i <<- i + 1L
    x[[i]]
  }

  new_iterator(iter)
}
