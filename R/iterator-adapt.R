#' Adapt an iterator with transformation steps
#'
#' @noRd
#' @description
#'
#' _Currently unexported because it is not clear whether we want to
#' commit to the transducer approach. Feedback welcome._
#'
#' `iter_adapt()` takes an iterator `iter` and a list of
#' [Transformation steps][steps]. It returns an iterator that returns
#' transformed values.
#'
#' `async_adapt()` does the same for async iterators, i.e. functions
#' that returns an awaitable value.
#'
#' @param iter An [iterator].
#' @param steps,... [Transformation steps][steps]. These dots are taken with
#'   implicit splicing of lists and passed to `compose()`.
#'
#' @seealso [loop()] for looping over iterator values with a `for`
#'   expression.
#' @examples
#' # Let's create a simple iterator:
#' iter <- as_iterator(1:50)
#' iter()
#'
#' # We can transform it by mapping functions over the elements:
#' iter <- iter_adapt(iter, iter_map(`+`, 100L))
#' take(iter, 5)
#'
#' # Or by discarding unwanted values:
#' iter <- iter_adapt(iter, iter_discard(~ .x %% 2 == 0))
#' take(iter, 5)
#'
#' # Note that iter_adapt() accepts several transformation at once and
#' # can of course adapt generators which are regular iterators:
#' iter <- gen(for (x in 1:10) yield(x))
#' iter <- iter_adapt(iter,
#'   iter_map(`+`, 100L),
#'   iter_discard(~ .x %% 2 == 0)
#' )
#' take(iter, 5)
iter_adapt <- function(iter, ...) {
  force(iter)

  steps <- compose(...)
  reducer <- steps(iter_builder)

  # Initialisation
  reducer()

  function() {
    flag <- "_coro_iter_adapt_result"
    last <- flag

    # If we get `flag` back, it means a transducer has skipped this
    # input. Continue until we get an actual result.
    while (identical(last, flag)) {
      if (is_exhausted(out <- iter())) {
        # Complete and terminate
        reducer(NULL)
        return(exhausted())
      }
      last <- reducer(flag, out)
    }

    last
  }
}
iter_builder <- function(result, input) {
  if (missing(result) || missing(input)) {
    NULL
  } else {
    input
  }
}

#' @noRd
#' @rdname iter_adapt
#' @name async_adapt
#' @usage async_adapt(iter, steps)
on_load(async_adapt %<~% async_generator(function(iter, steps) {
  force(iter)

  reducer <- steps(iter_builder)

  # Initialise the adaptors
  reducer()

  flag <- "_coro_iter_adapt_result"

  while (TRUE) {
    out <- await(iter())

    if (is_exhausted(out)) {
      # Finalise adaptors and signal exhaustion
      reducer(NULL)
      return(exhausted())
    }

    last <- reducer(flag, out)

    # If we get `flag` back, it means a transducer has skipped this
    # input. Continue until we get an actual result.
    if (identical(last, flag)) {
      next
    }

    yield(last)
  }
}))
