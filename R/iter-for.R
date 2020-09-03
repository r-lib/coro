#' Iterate over an iterator
#'
#' `iterate()` instruments `for` loops to support iteration with
#' flowery iterators and generators.
#'
#' @param loop A `for` loop expression.
#'
#' @seealso [iter_adapt()] for a more functional way of dealing with
#'   iterated valued.
#' @export
#' @examples
#' iter <- gen(for (x in 1:10) yield(x^2))
#' iterate(for (x in iter) {
#'   print(x * 100)
#' })
iterate <- function(loop) {
  loop <- substitute(loop)
  if (!is_call(loop, for_sym)) {
    abort("`loop` must be a `for` loop")
  }

  args <- node_cdr(loop)
  elt <- node_car(args)
  coll <- node_cadr(args)
  expr <- node_cadr(node_cdr(args))

  # Translate `for` loop to machine state
  reset_state()
  call <- for_call(elt, coll, expr)
  node <- as_exprs_node(call)
  parts <- node_list_parts(node)

  # Add breaking state to state machine
  node_list_poke_cdr(parts, pairlist(block(break_call())))

  # Wrap `while` in parens to disable JIT in case `env` is GlobalEnv
  call <- rlang::expr({
    (`while`)(TRUE, {
      !!machine_switch_call(parts)
    })
  })

  env <- caller_env()

  # Put machine state operators in scope temporarily
  local_bindings(.env = env, `_state` = 1L)

  invisible(eval_bare(call, env))
}

#' Iterable functions
#'
#' @description
#'
#' A flowery iterator is a function that implements the iteration
#' protocol:
#'
#' - The iterator is advanced by invoking it without argument. This
#'   returns the next value.
#'
#' - An iterator signals exhaustion by returning `NULL`.
#'
#'  The `NULL` sentinel synergises well with the R control flow
#' constructs like `while ()` as they return `NULL` when they are
#' done.
#'
#' @name iterator
NULL

#' @rdname iterator
#' @param x A vector to be transformed to an iterable function.
#'   Functions are returned as is.
#' @export
as_iterator <- function(x) {
  if (is_closure(x)) {
    return(x)
  }

  n <- length(x)
  i <- 0L

  function() {
    if (i == n) {
      return(NULL)
    }

    i <<- i + 1L
    x[[i]]
  }
}
