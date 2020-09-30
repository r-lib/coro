#' Adapt an iterator with transformation steps
#'
#' @description
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
#' @seealso [iterate()] for looping over iterator values with `for`
#'   loops.
#' @export
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
    flag <- "_flowery_iter_adapt_result"
    last <- flag

    # If we get `flag` back, it means a transducer has skipped this
    # input. Continue until we get an actual result.
    while (identical(last, flag)) {
      if (is_null(out <- iter())) {
        # Complete and terminate
        reducer(NULL)
        return(NULL)
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

#' @rdname iter_adapt
#' @export
#' @name async_adapt
#' @usage async_adapt(iter, steps)
on_load(async_adapt %<~% async_generator(function(iter, steps) {
  force(iter)

  reducer <- steps(iter_builder)

  # Initialise the adaptors
  reducer()

  flag <- "_flowery_iter_adapt_result"

  while (TRUE) {
    out <- await(iter())

    if (is_null(out)) {
      # Finalise adaptors and signal exhaustion
      reducer(NULL)
      return(NULL)
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
