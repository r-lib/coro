
into_builder <- function(to) {
  stopifnot(is_bare_vector(to))

  type <- typeof(to)
  coercer <- as_vector_fn(type)
  alloc <- new_vector_fn(type)

  # Reserve some space
  if (!length(to)) {
    to <- alloc(128L)
  }

  i <- 0L

  function(result, input) {
    if (missing(result)) {
      return(to[0])
    }
    if (missing(input)) {
      # Shrink vector if needed
      if (length(to) > i) {
        to <- to[seq_len(i)]
      }
      return(to)
    }

    next_i <- i + length(input)

    # Grow vector geometrically. Note that this incurs several copies,
    # extra tooling is needed in rlang to prevent this
    if (next_i > length(to)) {
      new_to <- alloc(ceiling(next_i * growth_rate))
      idx <- seq_len(i)
      new_to[idx] <- to[idx]
      to <<- new_to
    }

    to[seq2(i + 1, next_i)] <<- coercer(input)
    i <<- next_i

    to
  }
}
growth_rate <- 1.5
