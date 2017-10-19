
# Supports S3. Requires `[` and `[<-` methods and a supplied `coercer`.
into_builder <- function(to, coercer = NULL) {
  stopifnot(is_vector(to))

  if (is_null(coercer)) {
    if (is.object(to)) {
      abort("The `coercer` argument must be supplied for S3 objects")
    }
    coercer <- vector_coercer(to)
  }

  i <- 0L

  function(result, input) {
    if (missing(result)) {
      return(to[0])
    }
    if (missing(input)) {
      if (length(to) > i) {
        to <- to[seq_len(i)]
      }
      return(to)
    }

    n <- length(input)
    to[seq2(i + 1, i + n)] <<- coercer(input)
    i <<- i + n

    to
  }
}
vector_coercer <- function(x) {
  switch_type(x,
    logical = as.logical,
    integer = as.integer,
    double = as.double,
    complex = as.complex,
    string = as.character,
    character = as.character,
    raw = as.raw,
    list = as.list
  )
}
