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

  env <- caller_env()
  args <- node_cdr(loop)

  elt <- node_car(args)
  coll <- node_cadr(args)
  expr <- node_cadr(node_cdr(args))

  invisible(iter_for(!!elt, coll, !!expr, env = env))
}

iter_for <- function(elt, coll, expr, env = caller_env()) {
  lang <- for_call(enexpr(elt), coll, enexpr(expr))

  # Translate `for` loop to machine state
  reset_state()
  node <- as_exprs_node(lang)
  parts <- node_list_parts(node)

  # Add breaking state to state machine
  node_list_poke_cdr(parts, pairlist(block(break_call())))

  # Wrap `while` in parens to disable JIT in case `env` is GlobalEnv
  expr <- rlang::expr({
    (`while`)(TRUE, {
      !!machine_switch_call(parts)
    })
  })

  # Put machine state operators in scope temporarily
  local_bindings(.env = env, `_state` = 1L, !!!control_flow_ops)

  eval_bare(expr, env)
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
