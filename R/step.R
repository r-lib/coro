#' Transformation steps
#'
#' @description
#'
#' Transformation steps can be chained together to modify the
#' behaviour of an iterator (see [iter_adapt()]).
#'
#' * `map_step()` applies a function `.f` over all inputs.
#'
#' * `discard_step()` and `keep_step()` apply a predicate `.p` over
#'   inputs and discard or keep the selected elements.
#'
#' * `take_step()` is a transformation step that terminates early
#'   after `.n` inputs.
#'
#' @details
#'
#' Transformation steps, also called transducers, are function
#' operators that take a reducer function and return another reducer
#' function with modified behaviour.
#'
#' @name steps
#' @examples
#' # `purrr::compose()` is the recommended way to chain transformation
#' # steps:
#' compose <- purrr::compose
#' steps <- compose(map_step(`+`, 10), discard_step(`>`, 15))
NULL

#' @rdname steps
#' @param .n The number of inputs to take.
#' @export
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
        # because along_builder() does not expose intermediary
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
        as_box(result, "rlang_box_done")
      }
    }
  }
}

#' @rdname steps
#' @param .f A function to map over inputs. If needed this argument is
#'   transformed to a function with [rlang::as_closure()] and thus
#'   supports the lambda-formula notation.
#' @param ... Further arguments passed over to `.f` or `.p`.
#' @export
map_step <- function(.f, ...) {
  .f <- as_closure(.f)

  function(next_step) {
    force(next_step)

    function(result, input) {
      if (missing(result)) {
        return(next_step())
      }
      if (missing(input)) {
        return(next_step(result))
      }

      next_step(result, .f(input, ...))
    }
  }
}

#' @rdname steps
#' @param .p A predicate function applied to inputs. If needed this
#'   argument is transformed to a function with [rlang::as_closure()]
#'   and thus supports the lambda-formula notation.
#' @export
keep_step <- function(.p, ...) {
  .p <- as_closure(.p)
  discard_step(negate(.p), ...)
}
#' @rdname steps
#' @export
discard_step <- function(.p, ...) {
  .p <- as_closure(.p)

  function(next_step) {
    force(next_step)

    function(result, input) {
      if (missing(result)) {
        return(next_step())
      }
      if (missing(input)) {
        return(next_step(result))
      }

      if (.p(input, ...)) {
        result
      } else {
        next_step(result, input)
      }
    }
  }
}
