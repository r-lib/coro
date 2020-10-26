#' Transformation steps
#'
#' @noRd
#' @description
#'
#' Transformation steps can be chained together to modify the
#' behaviour of an iterator (see [iter_adapt()]).
#'
#' * `iter_map()` applies a function `.f` over all inputs.
#'
#' * `iter_discard()` and `iter_keep()` apply a predicate `.p` over
#'   inputs and discard or keep the selected elements.
#'
#' * `iter_take()` is a transformation step that terminates early
#'   after `n` inputs.
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
#' steps <- purrr::compose(iter_map(`+`, 10), iter_discard(`>`, 15))
NULL

#' @noRd
#' @rdname steps
#' @param n The number of inputs to take.
iter_take <- function(n) {
  force(n)

  function(continue) {
    # Forward `n` here to make it safe for mutation
    orig_n <- n
    curr_n <- n

    function(out, new) {
      if (missing(out)) {
        return(continue())
      }

      if (missing(new)) {
        # Check `out` only after it has been completely finalised
        # because along_builder() does not expose intermediary
        # results
        out <- continue(out)
        if (length(out) < orig_n) {
          abort(sprintf(
            "Not enough elements for `take()` \\
             (%s / %s elements)",
            length(out),
            orig_n
          ))
        }

        return(out)
      }

      curr_n <<- curr_n - 1L
      out <- continue(out, new)

      if (curr_n) {
        out
      } else {
        as_box(out, "rlang_box_done")
      }
    }
  }
}

#' @noRd
#' @rdname steps
#' @param .f A function to map over inputs. If needed this argument is
#'   transformed to a function with [rlang::as_closure()] and thus
#'   supports the lambda-formula notation.
#' @param ... Further arguments passed over to `.f` or `.p`.
iter_map <- function(.f, ...) {
  .f <- as_closure(.f)

  function(continue) {
    force(continue)

    function(out, new) {
      if (missing(out)) {
        return(continue())
      }
      if (missing(new)) {
        return(continue(out))
      }

      continue(out, .f(new, ...))
    }
  }
}

#' @noRd
#' @rdname steps
#' @param .p A predicate function applied to inputs. If needed this
#'   argument is transformed to a function with [rlang::as_closure()]
#'   and thus supports the lambda-formula notation.
iter_keep <- function(.p, ...) {
  .p <- as_closure(.p)
  iter_discard(negate(.p), ...)
}
#' @noRd
#' @rdname steps
iter_discard <- function(.p, ...) {
  .p <- as_closure(.p)

  function(continue) {
    force(continue)

    function(out, new) {
      if (missing(out)) {
        return(continue())
      }
      if (missing(new)) {
        return(continue(out))
      }

      if (.p(new, ...)) {
        out
      } else {
        continue(out, new)
      }
    }
  }
}
