
reset <- function(expr) {
  info <- list(expr = enexpr(expr), env = caller_env())

  value <- with_reset(expr, info)

  # Looped continuations end up here
  while (inherits(value, c("shift", "condition"))) {
    value <- with_reset(value$fn(value$cnt), info)
  }

  value
}
with_reset <- function(expr, info) {
  # FIXME: need safe quosures to use rlang::with_handlers()
  withCallingHandlers(tryCatch(expr, shift = identity),
    get_reset_info = restarting("reset_info", info)
  )
}

reset_info <- function() {
  info <- with_restarts(cnd_signal("get_reset_info"), reset_info = identity)

  if (is_null(info)) {
    abort("Can't shift outside of a reset")
  }

  info
}

# This is a reset operator that doesn't capture shift conditions. We
# use it instead of reset() to wrap continuations so that the stack
# doesn't grow on repeated invokation of continuations
continue <- function(expr, env) {
  expr <- enexpr(expr)
  info <- list(expr = expr, env = env)

  withCallingHandlers(eval_bare(expr, env),
    get_reset_info = restarting("reset_info", info)
  )
}


SHIFT <- function(fn) {
  info <- reset_info()

  expr <- duplicate(info$expr)

  cnt_body <- discard_past(expr)
  cnt_body <- splice_reset(cnt_body, info$env)
  cnt <- new_function(alist(`_next` = ), cnt_body, ns_env("flowery"))

  cnd_signal("shift", fn = fn, cnt = cnt)
}

as_exprs_node <- function(expr) {
  if (is_pairlist(expr)) {
    expr
  } else if (is_language(expr, quote(`{`))) {
    node_cdr(expr)
  } else {
    node(expr, NULL)
  }
}
splice_reset <- function(args, env) {
  curly <- new_language(quote(`{`), args)
  new_language(quote(continue), pairlist(curly, env = env))
}

discard_past <- function(expr) {
  args <- head <- as_exprs_node(expr)

  while (!is_null(args)) {
    arg <- node_car(args)
    arg <- expr_discard_past(arg)

    # Discarded expression
    if (is_null(arg)) {
      head <- args <- node_cdr(args)
      next
    }

    # Unassigned shift
    if (identical(arg, unassigned_shift)) {
      # If the shift is the last expression, keep it as a return
      # value. Otherwise, discard it
      if (is_null(node_cdr(args))) {
        head <- arg
      } else {
        head <- node_cdr(args)
      }
      break
    }

    # Assigned shift
    head <- arg
    args <- node_cdr(args)
    mut_node_tail_cdr(head, args)
    break
  }

  head
}
unassigned_shift <- pairlist(quote(UQ(`_next`)))

# Takes an expression and always returns a node
expr_discard_past <- function(expr) {
  if (is_language(expr, ctrl_syms)) {
    return(ctrl_discard_past(expr))
  }

  # If we have a shift, replace it with the next value and terminate
  if (is_shift(expr)) {
    args <- node(bangbang(next_sym), NULL)
    return(args)
  }
  if (is_assigned_shift(expr)) {
    assignment <- duplicate(expr)
    mut_node_cadr(node_cdr(assignment), bangbang(next_sym))
    args <- node(assignment, NULL)
    return(args)
  }

  if (is_language(expr)) {
    lang_check_shift(expr)
  }

  NULL
}

ctrl_discard_past <- function(expr) {
  nm <- as_string(node_car(expr))

  switch(nm,
    `if` = op_if_discard_past(expr),
    `while` = op_while_discard_past(expr),
    `repeat` = ,
    `for` = abort("Can't shift within a repeat or for loop. Please use while()"),
    abort(sprintf("Internal error: Unexpected control flow `%s`"), nm)
  )
}

op_if_discard_past <- function(expr) {
  branches <- node_cddr(expr)

  if (has_shift(node_car(branches))) {
    branch <- node_car(branches)
  } else if (has_shift(node_cadr(branches))) {
    branch <- node_cadr(branches)
  } else {
    branch <- NULL
  }

  discard_past(branch)
}
op_while_discard_past <- function(expr) {
  block <- node_cadr(node_cdr(expr))

  if (has_shift(block) && lang_has(block, is_jump)) {
    abort("Can't break or continue within a shifted loop")
  }

  # Extract remaining expressions within the loop
  args <- discard_past(block)

  # Duplicate because we might recurse in case of nested loops
  args <- duplicate(args)

  # Reenter loop at the end of current block
  loop <- duplicate(expr)
  mut_node_tail_cdr(args, node(loop, NULL))

  args
}
