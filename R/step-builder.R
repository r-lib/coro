#' Build a typed output
#'
#' @noRd
#' @description
#'
#' `along_builder()` and `poke_into_builder()` are builder functions,
#' i.e. reducer functions that construct an output when reduced with
#' [purrr::reduce()] or [reduce_steps()]. While `along_builder()`
#' duplicates its input, `poke_into_builder()` fills the supplied
#' input in place. The latter is only meant for experience users as
#' this mutation can cause unexpected side effects. You should only
#' mutate vectors that you have created.
#'
#' These builder functions construct a vector of a given type and
#' manage memory. The vector is automatically grown (with a rate of
#' 1.5) when it becomes too small and filled with the output during
#' reduction. The vector is automatically shrinked to the final output
#' size when the reduction completes. To avoid the extra copies caused
#' by vector growing and shrinking, supply a vector whose size matches
#' exactly the final output.
#'
#' The builder functions can be combined with [transformation
#' steps][steps] to provide interesting functionality. See the
#' [take()] variants for higher-level functions.
#'
#' @param along A vector that will be safely duplicated before
#'   filling.
#' @param to A vector that will be modified in place. If the final
#'   output has a different length than `to`, it will still be copied
#'   when grown and/or shrunk.
#'
#' @seealso [reduce_steps()], [steps]
#' @examples
#' # Let's discard all pair elements during reduction to make the
#' # examples a bit more interesting
#' trans <- iter_keep(~ . %% 2 == 0)
#'
#' # You can supply an empty vector to indicate the output type. The
#' # output vector will be grown efficiently.
#' reduce_steps(1:10, trans, along_builder(list()))
#'
#' # If you have created a vector and want to modify it in place to
#' # gain a bit of performance, you can use the poker version:
#' vec <- vector("list", 5L)
#' reduce_steps(1:10, trans, poke_into_builder(vec))
#'
#' # The output vector is modified in place. This can have surprising
#' # side effects:
#' vec
along_builder <- function(along) {
  to <- duplicate(along, shallow = TRUE)
  poke_into_builder(to)
}
#' @noRd
#' @rdname along_builder
poke_into_builder <- function(to) {
  stopifnot(is_bare_vector(to))

  type <- typeof(to)
  coercer <- as_vector_fn(type)
  alloc <- new_vector_fn(type)

  # Reserve some space
  if (!length(to)) {
    to <- alloc(128L)
  }

  i <- 0L

  function(out, new) {
    if (missing(out)) {
      return(to[0])
    }
    if (missing(new)) {
      # Shrink vector if needed
      if (length(to) > i) {
        to <- to[seq_len(i)]
      }
      return(to)
    }

    new <- coercer(new)
    next_i <- i + length(new)

    # Grow vector geometrically. Note that this incurs several copies,
    # extra tooling is needed in rlang to prevent this
    if (next_i > length(to)) {
      new_to <- alloc(ceiling(next_i * growth_rate))
      vec_poke_range(new_to, 1L, to, 1L, i)
      to <<- new_to
    }

    vec_poke_range(to, i + 1, new)
    i <<- next_i

    to
  }
}
growth_rate <- 1.5
