
new_iterator <- function(fn, length = NA, subclasses = chr()) {
  stopifnot(is_closure(fn))

  # Support logical `NA`
  if (is_na(length)) {
    length <- na_int
  } else if (is_scalar_double(length)) {
    length <- as_integer(length)
  } else if (!is_scalar_integer(length)) {
    abort("`length` must be a scalar integer")
  }

  # Flag so methods can check that they have an iterator
  `_flowery_iterator` <- TRUE

  stream <- is_na(length)
  done <- FALSE
  last <- NULL

  iter <- function() {
    if (done) {
      if (stream) {
        return(NULL)
      } else {
        abort("Batch iteration is done")
      }
    }

    last <<- fn()

    if (!stream) {
      length <<- length - 1L
    }
    done <<-
      (stream && is_null(last)) ||
      (!stream && !length)

    last
  }

  set_attrs(iter, class = c(subclasses, "iterator"))
}

is_iterator <- function(x) {
  inherits(x, "iterator")
}

deref <- function(x) {
  stopifnot(is_iterator(x))
  env_get(iter_env(x), "last")
}
is_done <- function(x) {
  stopifnot(is_iterator(x))
  env_get(iter_env(x), "done")
}
length.iterator <- function(x) {
  env_get(iter_env(x), "length")
}
remaining <- length

iter_env <- function(iter) {
  env <- get_env(iter)
  if (!env_has(env, "_flowery_iterator")) {
    abort("Expected an iterator")
  }
  env
}

is_batch_iterator <- function(x) {
  !is_na(length(x))
}
is_stream_iterator <- function(x) {
  is_na(length(x))
}

print.iterator <- function(x, ...) {
  if (is_na(length(x))) {
    cat("<stream-iterator>\n")
  } else {
    cat("<batch-iterator>\n")
  }
  fn <- env_get(iter_env(x), "fn")
  print(fn)

  invisible(x)
}

# Requires length() and `[[` methods
as_iterator <- function(x) {
  if (is_iterator(x)) {
    return(x)
  }
  if (is_closure(x)) {
    return(new_iterator(x))
  }

  i <- 0L

  iter <- function() {
    i <<- i + 1L
    x[[i]]
  }

  new_iterator(iter, length(x))
}

iterate <- function(expr, env = caller_env()) {
  expr <- enexpr(expr)
  if (!is_language(expr, for_sym)) {
    abort("`expr` must be a `for` loop")
  }
  args <- node_cdr(expr)

  iter <- eval_bare(node_cadr(args), env)
  iter <- as_iterator(iter)

  if (!is_iterator(iter)) {
    abort("`iterate()` expects a loop over an iterator")
  }

  elt_name <- as_string(node_car(args))
  loop_expr <- node_cadr(node_cdr(args))

  if (is_stream_iterator(iter)) {
    not_done <- function(iter) !is_null(iter())
    elt <- function(iter) deref(iter)
  } else {
    not_done <- function(iter) length(iter)
    elt <- function(iter) iter()
  }

  while (not_done(iter)) {
    env_poke(env, elt_name, elt(iter))
    eval_bare(loop_expr, env)
  }

  invisible(NULL)
}
