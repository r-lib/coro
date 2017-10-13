
new_iterator <- function(x, length = NA) {
  stopifnot(is_closure(x))
  stopifnot(is_na(length) || is_integerish(length))

  if (is_double(length)) {
    length <- as_integer(length)
  }

  set_attrs(x, class = "iterator", length = length)
}

deref <- function(x) {
  UseMethod("deref")
}
deref.iterator <- function(x) {
  abort("`deref()` method unimplemented")
}

length.iterator <- function(x) {
  n <- attr(x, "length")

  if (is_na(n)) {
    n <- with_restarts(iterator_length = identity,
      abort("Iterator has unknown length", "iterator_unknown_length")
    )
  }

  n
}

print.iterator <- function(x, ...) {
  cat("<iterator>\n")
  print(set_attrs(x, NULL))
  invisible(x)
}
print.batch_iterator <- function(x, ...) {
  cat("<batch-iterator>\n")
  print(set_attrs(x, NULL))
  cat(glue("{ length(x) } remaining iterations\n"))
  invisible(x)
}
print.stream_iterator <- function(x, ...) {
  cat("<stream-iterator>\n")
  print(set_attrs(x, NULL))
  invisible(x)
}

remaining <- length
