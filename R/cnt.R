
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

  while(!is_null(args)) {
    arg <- node_car(args)
    arg <- expr_discard_past(arg)

    if (is_null(arg)) {
      head <- node_cdr(args)
    } else {
      head <- arg
      args <- node_cdr(args)
      mut_node_tail_cdr(head, args)
      break
    }

    args <- node_cdr(args)
  }

  head
}

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
    `if` = if_discard_past(expr),
    `while` = while_discard_past(expr),
    abort("TODO ctrl")
  )
}
if_discard_past <- function(expr) {
  branches <- node_cddr(expr)

  if (has_shift(node_cadr(branches))) {
    branch <- node_cadr(branches)
  } else if (has_shift(node_cadr(node_cdr(branches)))) {
    branch <- node_caddr(node_cdr(branches))
  } else {
    branch <- NULL
  }

  discard_past(branch)
}
while_discard_past <- function(expr) {
  # Extract remaining expressions within the loop
  block <- node_cadr(node_cdr(expr))
  args <- discard_past(block)

  # Reenter loop at the end of current block
  loop <- duplicate(expr)
  mut_node_tail_cdr(args, node(loop, NULL))

  args
}

has_shift <- function(expr) {
  if (!is_language(expr)) {
    return(FALSE)
  }
  if (is_shift(expr) || is_assigned_shift(expr)) {
    return(TRUE)
  }

  head <- node_car(expr)
  if (has_shift(head)) {
    return(TRUE)
  }

  args <- node_cdr(expr)
  while (!is_null(args)) {
    if (has_shift(node_car(args))) {
      return(TRUE)
    }
    args <- node_cdr(args)
  }

  FALSE
}

is_shift <- function(expr) {
  is_language(expr, shift_sym)
}
is_assigned_shift <- function(expr) {
  is_language(expr, quote(`<-`)) && is_shift(node_cadr(node_cdr(expr)))
}

lang_check_shift <- function(lang) {
  if (is_assigned_shift(lang)) {
    return(NULL)
  }

  check_shift(node_car(lang))

  args <- node_cdr(lang)
  while (!is_null(args)) {
    car <- node_car(args)
    args <- node_cdr(args)

    check_shift(car)
  }
}
check_shift <- function(expr) {
  if (!is_language(expr)) {
    return(NULL)
  }

  if (is_shift(expr)) {
    abort("Can't shift within a function call")
  }
  lang_check_shift(expr)
}


bangbang <- function(expr) {
  lang(quote(UQ), expr)
}

ctrl_syms <- list(
  quote(`if`),
  quote(`for`),
  quote(`while`),
  quote(`repeat`)
)
next_sym <- quote(`_next`)
shift_sym <- quote(SHIFT)
