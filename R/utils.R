
vec_last <- function(x) {
  x[[length(x)]]
}
`vec_last<-` <- function(x, value) {
  x[[length(x)]] <- value
  x
}

map_last <- function(.x, .f, ...) {
  vec_last(.x) <- .f(vec_last(.x), ...)
  .x
}

set_class <- function(x, class) {
  set_attrs(x, class = class)
}
