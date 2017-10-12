#' Create a generator factory
#'
#' @param fn A function that serves as mold for creating new
#'   generators.
#' @export
new_gen_factory <- function(fn) {
  factory <- expr_interp(function() {
    args <- list(!!! fn_fmls_syms(fn))
    new_generator(body(fn), environment(fn), args)
  })

  formals(factory) <- fn_fmls(fn)
  factory
}

#' Create a generator
#'
#' @param body The body of the generator. It should use the `yield()`
#'   keyword to pause the generator.
#' @param env The lexical scope of the generator.
#' @param args Variables passed to the generator.
#' @export
new_generator <- function(body, env, args = list()) {
  parts <- machine_parts(body)
  if (is_null(parts)) {
    abort("Generators must call `yield()`")
  }

  env <- gen_env(env, args)

  expr_interp(function() {
    evalq(env, expr = {
      while (TRUE) {
        !! machine_switch_lang(parts)
      }
    })
  })
}

machine_switch_lang <- function(parts) {
  parts <- node_list_enumerate_tags(parts)
  switch_args <- node(quote(`_state`), parts)
  new_language(switch_sym, switch_args)
}
node_list_enumerate_tags <- function(node) {
  i <- 0L
  rest <- node

  while (!is_null(rest)) {
    i <- i + 1L
    node_poke_tag(rest, sym(as.character(i)))
    rest <- node_cdr(rest)
  }

  invisible(node)
}

gen_env <- function(env, args) {
  env <- env_bury(env, !!! control_flow_ops)
  env <- env_bury(env, `_state` = "1", !!! args)
  env
}

`_goto` <- function(state, frame = caller_env()) {
  env_poke(frame, "_state", state)
  eval_bare(next_lang(), frame)
}
`_pause` <- function(state, value = NULL, frame = caller_env()) {
  env_poke(frame, "_state", state)
  eval_bare(return_lang(value), frame)
}
control_flow_ops <- list(`_goto` = `_goto`, `_pause` = `_pause`)
