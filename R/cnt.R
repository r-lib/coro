
reset <- function(expr) {
  info <- list(
    frame = ctxt_frame(),
    env = caller_env(),
    expr = enexpr(expr)
  )

  ## TODO: need safe quosures
  ## The quosure should be evaluated in its environment, not in overscope
  # with_handlers(!! expr,
  #   shift = exiting(function(cnd) cnd$result),
  #   get_reset_info = restarting("reset_info", info)
  # )

  withCallingHandlers(tryCatch(expr, shift = function(cnd) cnd$result),
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


SHIFT <- function(fn) {
  info <- reset_info()

  cnt_body <- discard_past(info$expr)
  cnt <- new_function(alist(`_next` = ), cnt_body, env = info$env)

  result <- fn(cnt)
  cnd_signal("shift", result = result)
}

discard_past <- function(expr) {
  expr <- duplicate(expr)

  if (!is_language(expr)) {
    return(expr)
  }

  car <- node_car(expr)
  if (is_language(car) || !is_symbol(car)) {
    lang_check_yield(car)
    return(NULL)
  }

  nm <- as_string(car)
  switch(nm,
    SHIFT =
      abort("TODO"),
    `{` =
      curly_discard_past(expr),
    `if` = ,
    `for` = ,
    `while` =
      ctrl_discard_past(expr),
    lang_check_yield(expr)
  )

  expr
}

curly_discard_past <- function(curly) {
  args <- node_cdr(curly)

  while(!is_null(args)) {
    car <- node_car(args)

    if (is_language(car, ctrl_syms)) {
      new_args <- ctrl_discard_past(car)
      mut_node_tail_cdr(new_args, node_cdr(args))
      mut_node_cdr(curly, new_args)
      break
    }

    if (is_language(car)) {
      lang_check_yield(car)
    }

    # If we have a shift, replace it with the next value and terminate
    if (is_shift(car)) {
      mut_node_car(args, next_sym)
      mut_node_cdr(curly, args)
      break
    }
    if (is_assigned_shift(car)) {
      mut_node_cadr(node_cdr(car), next_sym)
      mut_node_cdr(curly, args)
      break
    }

    args <- node_cdr(args)
    mut_node_cdr(curly, args)
  }
}

# Can discard if else branches etc
ctrl_discard_past <- function(expr) {
  nm <- as_string(node_car(expr))

  expr <- switch(nm,
    `if` = if_discard_past(expr),
    abort("TODO ctrl")
  )

  as_exprs_node(expr)
}
as_exprs_node <- function(expr) {
  if (is_language(expr, quote(`{`))) {
    node_cdr(expr)
  } else {
    node(expr, NULL)
  }
}

if_discard_past <- function(expr) {
  branches <- node_cddr(expr)

  if (has_shift(node_cadr(branches))) {
    branch <- node_cadr(branches)
  } else if (has_shift(node_caddr(branches))) {
    branch <- node_caddr(branches)
  } else {
    branch <- NULL
  }

  discard_past(branch)
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

lang_check_yield <- function(lang) {
  if (is_assigned_shift(lang)) {
    return(NULL)
  }

  check_yield(node_car(lang))

  args <- node_cdr(lang)
  while (!is_null(args)) {
    car <- node_car(args)
    args <- node_cdr(args)

    check_yield(car)
  }
}
check_yield <- function(expr) {
  if (!is_language(expr)) {
    return(NULL)
  }

  if (is_shift(expr)) {
    abort("Can't shift within a function call")
  }
  lang_check_yield(expr)
}


ctrl_syms <- list(
  quote(`if`),
  quote(`for`),
  quote(`while`),
  quote(`repeat`)
)
next_sym <- quote(`_next`)
shift_sym <- quote(SHIFT)
