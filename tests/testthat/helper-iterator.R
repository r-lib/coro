
simple_iter <- new_iterator(function() "body")

new_integer_iterator <- function() {
  i <- 0L
  new_iterator(function() {
    out <- i
    i <<- i + 1L
    out
  })
}
