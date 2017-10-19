
take_step <- function(.n) {
  force(.n)

  function(next_step) {
    # Forward `.n` here to make it safe for mutation
    n <- .n

    function(result, input) {
      if (missing(result)) {
        return(next_step())
      }

      if (missing(input)) {
        # Check `result` only after it has been completely finalised
        # because into_init_step() does not expose intermediary
        # results
        result <- next_step(result)
        if (length(result) < .n) {
          abort(glue(
            "Not enough elements for `take()` \\
             ({ length(result) } / { .n } elements)"
          ))
        }

        return(result)
      }

      n <<- n - 1L
      result <- next_step(result, input)

      if (n) {
        result
      } else {
        ensure_reduced(result)
      }
    }
  }
}
