
generator_body <- function(fn) {
  info <- machine_info("generator", env = caller_env())
  walk_states(body(fn), info = info)
}

async_body <- function(fn) {
  info <- machine_info("async", env = caller_env())
  walk_states(body(fn), info = info)
}

machine_parts <- function(expr, state = NULL) {
  reset_state(state)

  fmls <- formals(expr)
  if (length(fmls) == 1) {
    poke_state_elt("arg_sym", sym(names(fmls)))
  }

  expr <- set_returns(expr)
  generator_parts(expr)
}

return_invisible_call <- return_state_call(rlang::call2("invisible", NULL))

new_for_parts <- function(state, i, x,
                          next_state = NULL,
                          body = quote(yield())) {
  for_call <- for_call(i, x, body)
  parts <- with_state(state, for_parts(for_call))

  if (!is_null(next_state)) {
    cond_branches <- node_cddr(node_cadr(node_cadr(parts)))
    goto_block <- node_cadr(cond_branches)
    node_poke_cadr(goto_block, goto_call(next_state))
  }

  node_poke_cddr(parts, NULL)
  parts
}

print_parts_refs <- function(fn) {
  parts <- machine_parts(fn)

  for (i in seq_along(parts)) {
    refs <- parts_srcrefs(parts[[i]])

    if (!is_null(refs)) {
      writeLines(sprintf("<Part %s>", i))
      print(refs)
    }
  }
}
