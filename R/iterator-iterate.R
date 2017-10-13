
iterate <- function(loop) {
  loop <- enexpr(loop)
  if (!is_language(loop, for_sym)) {
    abort("`loop` must be a `for` loop")
  }
  env <- caller_env()

  args <- node_cdr(loop)
  iter <- eval_bare(node_cadr(args), env)
  iter <- as_iterator(iter)

  if (!is_iterator(iter)) {
    abort("`iterate()` expects a loop over an iterator")
  }

  elt_name <- as_string(node_car(args))
  loop_expr <- node_cadr(node_cdr(args))
  loop_expr <- poke_loop_keywords(loop_expr)

  if (is_stream_iterator(iter)) {
    not_done <- function(iter) !is_null(iter())
    elt <- function(iter) deref(iter)
  } else {
    not_done <- function(iter) length(iter)
    elt <- function(iter) iter()
  }

  while (not_done(iter)) {
    env_poke(env, elt_name, elt(iter))
    out <- with_loop_flow(eval_bare(loop_expr, env))
    switch(out,
      `next` = next,
      `break` = break
    )
  }

  invisible(NULL)
}

poke_loop_keywords <- function(node) {
  rest <- node

  while (TRUE) {
    rest <- node_cdr(rest)
    if (is_null(rest)) {
      break
    }

    current <- node_car(rest)
    if (!is_language(current) || !is_symbol(node_car(current))) {
      next
    }

    head <- node_car(current)
    switch(as_string(head),
      # Don't poke within nested loops
      `repeat` = ,
      `while` = ,
      `for` = {
        next
      },
      `next` = {
        node_poke_car(rest, flow_signaller("flowery_next"))
        next
      },
      `break` = {
        node_poke_car(rest, flow_signaller("flowery_break"))
        next
      },
      poke_loop_keywords(current)
    )
  }

  invisible(node)
}

flow_signaller <- function(type) {
  lang(function() cnd_signal(type))
}

with_loop_flow <- function(expr, env = caller_env()) {
  tryCatch(
    flowery_next = function(cnd) "next",
    flowery_break = function(cnd) "break",
    {
      expr
      "next"
    }
  )
}
