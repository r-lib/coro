
new_iterator <- function(x, length = na_int, subclasses = chr()) {
  stopifnot(is_closure(x))

  # Support logical `NA`
  if (is_na(length)) {
    length <- na_int
  } else if (is_scalar_double(length)) {
    length <- as_integer(length)
  } else if (!is_scalar_integer(length)) {
    abort("`length` must be a scalar integer")
  }

  class <- c(subclasses, "iterator")

  set_attrs(x, class = class, length = length)
}

deref <- function(x) {
  UseMethod("deref")
}
deref.iterator <- function(x) {
  abort("Can't dereference bare iterators")
}

length.iterator <- function(x) {
  attr(x, "length")
}
remaining <- length

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
  print(set_attrs(x, NULL))

  invisible(x)
}
