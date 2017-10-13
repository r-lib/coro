
stream_iter <- new_iterator(function() "body")
batch_iter <- new_iterator(function() "body", 10L)

new_integer_stream <- function() {
  i <- 0L
  function() {
    out <- i
    i <<- i + 1L
    out
  }
}
