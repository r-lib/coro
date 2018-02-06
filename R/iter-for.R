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
  loop <- enexpr(loop)
  if (!is_call(loop, for_sym)) {
    abort("`loop` must be a `for` loop")
  }

  env <- caller_env()
  args <- node_cdr(loop)

  elt <- node_car(args)
  coll <- node_cadr(args)
  expr <- node_cadr(node_cdr(args))

  iter_for(!! elt, coll, !! expr, env = env)
}

iter_for <- function(elt, coll, expr, env = caller_env()) {
  lang <- for_lang(enexpr(elt), coll, enexpr(expr))

  # Translate `for` loop to machine state
  reset_state()
  node <- as_exprs_node(lang)
  parts <- node_list_parts(node)

  # Add breaking state to state machine
  node_list_poke_cdr(parts, node_list(block(break_lang())))

  # Wrap `while` in parens to disable JIT in case `env` is GlobalEnv
  expr <- rlang::expr({
    (`while`)(TRUE, {
      !! machine_switch_lang(parts)
    })
  })

  # Put machine state operators in scope temporarily
  scoped_bindings(.env = env, `_state` = 1L, !!! control_flow_ops)

  eval_bare(expr, env)
}
