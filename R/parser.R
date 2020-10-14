
walk_states <- function(expr, info) {
  continue <- function(counter, last) {
    # Break if last
    if (last) 0L else counter() + 1L
  }
  states <- expr_states(
    expr,
    counter = new_counter(1L),
    continue = continue,
    last = TRUE,
    return = TRUE,
    info = info
  )

  expr({
    if (exhausted) {
      return(invisible(NULL))
    }
    repeat switch(state[[1L]], !!!states)

    exhausted <- TRUE
    invisible(NULL)
  })
}
walk_loop_states <- function(body, states, counter, info) {
  continue <- function(counter, last) {
    # Go back to state 1 of loop body if last
    if (last) 1L else counter() + 1L
  }

  loop_depth <- machine_depth(counter)

  nested_states <- expr_states(
    body,
    counter,
    continue = continue,
    last = TRUE,
    return = FALSE,
    info = info
  )
  states <- node_list_poke_cdr(states, nested_states)

  expr(repeat switch(state[[!!loop_depth]], !!!states))
}
walk_branch_states <- function(body, offset, counter, continue, last, return, info) {
  nested_continue <- function(counter, last) {
    counter() + 1L
  }

  prev_depth <- machine_depth(counter)
  machine_depth <- prev_depth + 1L
  loop_depth <- loop_depth(counter, check = FALSE)
  nested_counter <- new_counter(machine_depth, loop_depth = loop_depth)

  states <- expr_states(
    body,
    nested_counter,
    continue = nested_continue,
    last = last,
    return = return,
    info = info
  )

  breaking_state <- new_state(quote({ break }), NULL, nested_counter())
  states <- node_list_poke_cdr(states, breaking_state)
  nested_counter(inc = 1L)

  # Don't add offset if state is breaking
  next_i <- continue(counter, last)
  if (next_i) {
    next_i <- next_i + offset
  }

  expr({
    repeat switch(state[[!!machine_depth]], !!!states)
    n <- length(state)
    if (n < !!prev_depth) {
      break
    }
    if (n == !!prev_depth) {
      state[[!!prev_depth]] <- 1L
      next
    }
    length(state) <- !!prev_depth

    !!continue_call(next_i, prev_depth)
  })
}

expr_states <- function(expr, counter, continue, last, return, info) {
  switch(expr_type(expr),
    `{` = block_states(
      block = expr,
      counter = counter,
      continue = continue,
      last = last,
      return = return,
      info = info
    ),
    `expr` = continue_state(
      expr = user_call(expr),
      counter = counter,
      continue = continue,
      last = last,
      return = return,
      info = info
    ),
    `yield` = yield_state(
      expr = user_call(node_get(expr, 2)),
      counter = counter,
      continue = continue,
      last = last,
      return = return,
      info = info
    ),
    `await` = await_state(
      expr = user_call(node_get(expr, 2)),
      counter = counter,
      continue = continue,
      last = last,
      return = return,
      info = info
    ),
    `yield_assign` = yield_assign_states(
      expr = user_call(node_get(node_get(expr, 3), 2)),
      var = as_string(node_get(expr, 2)),
      counter = counter,
      continue = continue,
      last = last,
      return = return,
      info = info
    ),
    `await_assign` = await_assign_states(
      expr = user_call(node_get(node_get(expr, 3), 2)),
      var = as_string(node_get(expr, 2)),
      counter = counter,
      continue = continue,
      last = last,
      return = return,
      info = info
    ),
    `return` = return_state(
      expr = user_call(strip_explicit_return(expr)),
      counter = counter,
      info = info
    ),
    `if` = if_states(
      preamble = NULL,
      condition = user_call(node_cadr(expr)),
      then_body = node_cadr(node_cdr(expr)),
      else_body = node_cadr(node_cddr(expr)),
      counter = counter,
      continue = continue,
      last = last,
      return = return,
      info = info
    ),
    `break` = break_state(
      preamble = NULL,
      counter = counter,
      info = info
    ),
    `next` = next_state(
      preamble = NULL,
      counter = counter,
      info = info
    ),
    `repeat` = loop_states(
      preamble = NULL,
      init = NULL,
      body = node_cadr(expr),
      cleanup = NULL,
      counter = counter,
      nested_counter = NULL,
      nested_states = NULL,
      continue = continue,
      last = last,
      info = info
    ),
    `while` = while_states(
      preamble = NULL,
      condition = user_call(node_cadr(expr)),
      body = node_cadr(node_cdr(expr)),
      counter = counter,
      continue = continue,
      last = last,
      info = info
    ),
    `for` = for_states(
      preamble = NULL,
      var = node_cadr(expr),
      iterator = node_cadr(node_cdr(expr)),
      body = node_cadr(node_cddr(expr)),
      counter = counter,
      continue = continue,
      last = last,
      info = info
    ),
    `tryCatch` = ,
    `on.exit` = stop_internal("expr_states", sprintf("Unimplemented operation `%s`", expr_type(expr))),
    stop_internal("expr_states", sprintf("Unexpected operation `%s`", expr_type(expr)))
  )
}

expr_type <- function(expr) {
  default <- "expr"

  if (!is_call(expr)) {
    return(default)
  }

  head <- node_car(expr)
  if (is_call(head, "::") && identical(head[[2]], quote(flowery))) {
    head <- head[[3]]
    if (!as_string(head) %in% c("yield", "await", "await_each")) {
      return(default)
    }
  }

  if (!is_symbol(head)) {
    return(default)
  }
  head <- as_string(head)

  if (is_string(head, "<-")) {
    rhs <- node_cadr(node_cdr(expr))
    if (is_yield_call(rhs)) {
      return("yield_assign")
    } else if (is_await_call(rhs)) {
      return("await_assign")
      return(default)
    }
  }

  switch(head,
    `{` = ,
    `yield` = ,
    `await` = ,
    `return` = ,
    `if` = ,
    `repeat` = ,
    `while` = ,
    `for` = ,
    `break` = ,
    `next` = ,
    `tryCatch` = ,
    `on.exit` = head,
    default
  )
}

block_states <- function(block, counter, continue, last, return, info) {
  # Only the last expression of `block` is returnable
  saved_last <- last
  last <- FALSE

  block <- duplicate(block, shallow = TRUE)
  refs <- block_refs(block)
  states <- NULL

  node <- node_cdr(block)

  # Handle to previous node so we can pop `node` from the current
  # pairlist
  prev_node <- NULL
  prev_refs <- NULL

  # Handle to the current head of pairlists of expressions. New user
  # blocks are created starting from this handle.
  curr_node <- node
  curr_refs <- refs

  accum <- function(go) {
    prev_node <<- node
    prev_refs <<- refs
    node <<- node_cdr(node)
    refs <<- node_cdr(refs)

    go
  }
  splice <- function(nested_node, nested_refs) {
    node_list_poke_cdr(nested_node, node_cdr(node))
    node_list_poke_cdr(nested_refs, node_cdr(refs))

    if (is_null(prev_node)) {
      curr_node <<- nested_node
      curr_refs <<- nested_refs
    } else {
      node_poke_cdr(prev_node, nested_node)
      node_poke_cdr(prev_refs, nested_refs)
    }

    node <<- nested_node
    refs <<- nested_refs
  }
  collect <- function() {
    if (is_null(curr_node)) {
      # Happens on skip
      curr_node <<- node
      curr_refs <<- refs
      return()
    }

    next_node <- node_cdr(node)
    next_refs <- node_cdr(refs)

    node <<- node_poke_cdr(node, NULL)
    refs <<- node_poke_cdr(node, NULL)

    out <- new_refd_block(curr_node, curr_refs)

    prev_node <<- NULL
    prev_refs <<- NULL
    node <<- curr_node <<- next_node
    refs <<- curr_refs <<- next_refs

    out
  }
  skip <- function() {
    if (is_null(prev_node)) {
      curr_node <<- NULL
      curr_refs <<- NULL
      node <<- node_cdr(node)
      refs <<- node_cdr(refs)
    } else {
      node_poke_cdr(prev_node, NULL)
      node_poke_cdr(prev_refs, NULL)
    }
  }

  push_states <- function(state) {
    states <<- node_list_poke_cdr(states, state)
  }

  # Collect as many user expressions as possible
  while (!is_null(node)) {
    # Set last expression of `block` as returnable
    if (is_null(node_cdr(node))) {
      last <- saved_last
    }

    expr <- node_car(node)
    ref <- node_car(refs)
    type <- expr_type(expr)

    switch(type,
      `expr` = {
        accum(next)
      },
      `{` = {
        nested_node <- node_cdr(expr)
        nested_refs <- block_refs(expr)
        splice(nested_node, nested_refs)
        next
      },
      `yield` = {
        node_poke_car(node, node_get(expr, 2))
        push_states(yield_state(
          expr = collect(),
          counter = counter,
          continue = continue,
          last = last,
          return = return,
          info = info
        ))
        next
      },
      `yield_assign` = {
        node_poke_car(node, node_get(node_get(expr, 3), 2))
        push_states(yield_assign_states(
          expr = collect(),
          var = as_string(node_get(expr, 2)),
          counter = counter,
          continue = continue,
          last = last,
          return = return,
          info = info
        ))
        next
      },
      `await` = {
        node_poke_car(node, node_get(expr, 2))
        push_states(await_state(
          expr = collect(),
          counter = counter,
          continue = continue,
          last = last,
          return = return,
          info = info
        ))
        next
      },
      `await_assign` = {
        node_poke_car(node, node_get(node_get(expr, 3), 2))
        push_states(await_assign_states(
          expr = collect(),
          var = as_string(node_get(expr, 2)),
          counter = counter,
          continue = continue,
          last = last,
          return = return,
          info = info
        ))
        next
      },
      `return` = {
        node_poke_car(node, strip_explicit_return(expr))
        push_states(return_state(
          expr = collect(),
          counter = counter,
          info = info
        ))
        next
      },
      `if` = {
        skip()
        push_states(if_states(
          preamble = collect(),
          condition = user_call(refd_block(node_cadr(expr), ref)),
          then_body = node_cadr(node_cdr(expr)),
          else_body = node_cadr(node_cddr(expr)),
          counter = counter,
          continue = continue,
          last = last,
          return = return,
          info = info
        ))
        next
      },
      `repeat` = {
        node_poke_car(node, "repeat")
        push_states(loop_states(
          preamble = collect(),
          init = NULL,
          body = node_cadr(expr),
          cleanup = NULL,
          counter = counter,
          nested_counter = NULL,
          nested_states = NULL,
          continue = continue,
          last = last,
          info = info
        ))
        next
      },
      `while` = {
        skip()
        push_states(while_states(
          preamble = collect(),
          condition = user_call(refd_block(node_cadr(expr), ref)),
          body = node_cadr(node_cdr(expr)),
          counter = counter,
          continue = continue,
          last = last,
          info = info
        ))
        next
      },
      `for` = {
        skip()
        push_states(for_states(
          preamble = collect(),
          var = node_cadr(expr),
          iterator = node_cadr(node_cdr(expr)),
          body = node_cadr(node_cddr(expr)),
          counter = counter,
          continue = continue,
          last = last,
          info = info
        ))
        next
      },
      `break` = {
        node_poke_car(node, "break")
        push_states(break_state(
          preamble = collect(),
          counter = counter,
          info = info
        ))
        next
      },
      `next` = {
        node_poke_car(node, "next")
        push_states(next_state(
          preamble = collect(),
          counter = counter,
          info = info
        ))
        next
      }
    )

    abort(sprintf("TODO in `block_states()`: %s", type))
  }

  last <- saved_last

  if (!is_null(curr_node)) {
    block <- new_refd_block(curr_node, curr_refs)
    push_states(continue_state(
      expr = block,
      counter = counter,
      continue = continue,
      last = last,
      return = return,
      info = info
    ))
  }

  states
}

return_state <- function(expr, counter, info) {
  block <- expr({
    !!expr
    exhausted <- TRUE
    !!return_call(info)
  })
  state <- new_state(block, NULL, tag = counter())
  counter(inc = 1L)

  state
}
strip_explicit_return <- function(expr) {
  if (is_call(expr, "return")) {
    return(node_cadr(expr))
  }

  if (is_call(expr, "{") && is_call(node_list_tail(expr), "return")) {
    expr <- duplicate(expr, shallow = TRUE)
    tail <- node_list_tail(expr)
    node_poke_car(tail, strip_explicit_return(node_car(tail)))
  }

  expr
}
return_call <- function(info) {
  if (is_null(info$async_ops)) {
    quote(return(last_value()))
  } else {
    quote(return(as_promise(last_value())))
  }
}

continue_state <- function(expr, counter, continue, last, return, info) {
  if (last && return) {
    return(return_state(expr, counter, info))
  }

  i <- counter()
  next_i <- continue(counter, last)

  block <- expr({
    !!expr
    !!continue_call(next_i, machine_depth(counter))
  })
  state <- new_state(block, NULL, tag = i)
  counter(inc = 1L)

  state
}

yield_state <- function(expr, counter, continue, last, return, info) {
  assert_can_yield(info)
  expr <- expr(validate_yield(!!expr))

  if (is_string(info$type, "async_generator")) {
    expr <- expr(.last_value <- as_promise(!!expr))
  }

  suspend_state(expr, counter, continue, last, return, info)
}
assert_can_yield <- function(info) {
  if (is_string(info$type, "async")) {
    abort("Can't yield within an `async()` function.")
  }
}

suspend_state <- function(expr, counter, continue, last, return, info) {
  if (last && return) {
    return(return_state(expr, counter, info))
  }

  i <- counter()
  next_i <- continue(counter, last)
  depth <- machine_depth(counter)

  block <- expr({
    !!expr
    state[[!!depth]] <- !!next_i
    suspend()
    return(last_value())
  })
  state <- new_state(block, NULL, tag = i)
  counter(inc = 1L)

  state
}

yield_assign_states <- function(expr,
                                var,
                                counter,
                                continue,
                                last,
                                return,
                                info) {
  assert_can_yield(info)
  expr <- expr(validate_yield(!!expr))
  suspend_assign_states(expr, var, counter, continue, last, return, info)
}

suspend_assign_states <- function(expr,
                                  var,
                                  counter,
                                  continue,
                                  last,
                                  return,
                                  info) {
  # Hard-code `last` to `FALSE` because we are inserting an assignment
  # state after the yielding state
  states <- suspend_state(expr, counter, continue, last = FALSE, return = return)

  assign_block <- expr({
    user_env[[!!var]] <- arg
    !!continue_call(continue(counter, last), machine_depth(counter))
  })
  assign_state <- new_state(assign_block, NULL, counter())
  node_list_poke_cdr(states, assign_state)
  counter(inc = 1L)

  states
}

await_state <- function(expr, counter, continue, last, return, info) {
  expr <- expr(.last_value <- then(as_promise(!!expr), callback = .self))
  suspend_state(
    expr = expr,
    counter = counter,
    continue = continue,
    last = last,
    return = return,
    info = info
  )
}

await_assign_states <- function(expr,
                                var,
                                counter,
                                continue,
                                last,
                                return,
                                info) {
  expr <- expr(.last_value <- then(as_promise(!!expr), callback = .self))
  suspend_assign_states(expr, var, counter, continue, last, return, info)
}

if_states <- function(preamble,
                      condition,
                      then_body,
                      else_body,
                      counter,
                      continue,
                      last,
                      return,
                      info) {
  i <- counter()
  i_then <- i + 1L
  i_else <- i_then + 1L

  depth <- machine_depth(counter)

  if_block <- expr({
    !!!preamble %&&% list(user_call(preamble))
    if (!!condition) {
      state[[!!depth]] <- !!i_then
    } else {
      state[[!!depth]] <- !!i_else
    }
    state[[!!(depth + 1L)]] <- 1L
  })
  states <- new_state(if_block, NULL, i)
  counter(inc = 1L)

  offset <- if (is_null(else_body)) 0L else 1L
  then_machine <- walk_branch_states(then_body, offset, counter, continue, last, return, info)
  then_state <- new_state(then_machine, NULL, i_then)
  states <- node_list_poke_cdr(states, then_state)
  counter(inc = 1L)

  if (!is_null(else_body)) {
    else_machine <- walk_branch_states(else_body, 0L, counter, continue, last, return, info)
    else_state <- new_state(else_machine, NULL, i_else)
    states <- node_list_poke_cdr(states, else_state)
    counter(inc = 1L)
  }

  if (last) {
    after_block <- block(continue_call(continue(counter, last), depth))
    after_state <- new_state(after_block, NULL, counter())
    states <- node_list_poke_cdr(states, after_state)
    counter(inc = 1L)
  }

  states
}

loop_states <- function(preamble,
                        init,
                        body,
                        cleanup,
                        counter,
                        nested_counter,
                        nested_states,
                        continue,
                        last,
                        info) {
  states <- NULL
  i <- counter()
  next_i <- i + 1L
  depth <- machine_depth(counter)

  preamble_block <- expr({
    !!!preamble %&&% list(user_call(preamble))
    !!!init %&&% list(init)
    state[[!!depth]] <- !!next_i
    state[[!!(depth + 1L)]] <- 1L
  })

  preamble_state <- new_state(preamble_block, NULL, i)
  states <- node_list_poke_cdr(states, preamble_state)
  i <- counter(inc = 1L)
  next_i <- continue(counter, last)

  loop_depth <- depth + 1L
  nested_counter <- nested_counter %||% new_counter(loop_depth, loop_depth = loop_depth)

  nested_machine_block <- expr({
    !!walk_loop_states(body, nested_states, nested_counter, info = info)
    !!!cleanup %&&% list(cleanup)
    length(state) <- !!depth
    !!continue_call(next_i, depth)
  })
  nested_machine_state <- new_state(nested_machine_block, NULL, i)
  states <- node_list_poke_cdr(states, nested_machine_state)
  counter(inc = 1L)

  states
}

while_states <- function(preamble,
                         condition,
                         body,
                         counter,
                         continue,
                         last,
                         info) {
  nested_counter <- new_loop_counter(counter)
  nested_states <- condition_state(condition, nested_counter)

  loop_states(
    preamble = preamble,
    init = NULL,
    body = body,
    cleanup = NULL,
    counter = counter,
    nested_counter = nested_counter,
    nested_states = nested_states,
    continue = continue,
    last = last,
    info = info
  )
}

for_states <- function(preamble,
                       var,
                       iterator,
                       body,
                       counter,
                       continue,
                       last,
                       info) {
  loop_depth <- machine_depth(counter) + 1L

  async <- is_call(iterator, "await_each", ns = c("", "flowery"))
  if (async) {
    if (is_string(info$type, "generator")) {
      abort(c(
        "Can't use `await_each()` in a non-async generator.",
        i = "Do you need `async_generator()`?"
      ))
    }
    iterator <- iterator[[2]]
  }

  init <- expr(
    iterators[[!!loop_depth]] <- as_iterator(!!user_call(iterator))
  )

  nested_counter <- new_loop_counter(counter)

  if (async) {
    await_block <- expr({
      iterator <- iterators[[!!loop_depth]]
      iterator()
    })
    nested_states <- await_state(await_block, nested_counter, continue, FALSE, FALSE, info)

    condition <- expr({
      if (is_null(arg)) {
        FALSE
      } else {
        user_env[[!!as.character(var)]] <- arg
        TRUE
      }
    })
    condition_state <- condition_state(condition, nested_counter)
    nested_states <- node_list_poke_cdr(nested_states, condition_state)
  } else {
    condition <- expr({
      iterator <- iterators[[!!loop_depth]]
      if (is_null(elt <- iterator())) {
        FALSE
      } else {
        user_env[[!!as.character(var)]] <- elt
        TRUE
      }
    })
    nested_states <- condition_state(condition, nested_counter)
  }

  cleanup <- expr(
    iterators[[!!loop_depth]] <- NULL
  )

  loop_states(
    preamble = preamble,
    init = init,
    body = body,
    cleanup = cleanup,
    counter = counter,
    nested_counter = nested_counter,
    nested_states = nested_states,
    continue = continue,
    last = last,
    info = info
  )
}

condition_state <- function(condition, counter) {
  i <- counter()
  next_i <- i + 1L
  loop_depth <- machine_depth(counter)

  block <- block(expr(
    if (!!condition) {
      state[[!!loop_depth]] <- !!next_i
    } else {
      break
    }
  ))
  state <- new_state(block, NULL, i)
  counter(inc = 1L)

  state
}

break_state <- function(preamble, counter, info) {
  loop_depth <- loop_depth(counter)

  if (loop_depth == machine_depth(counter)) {
    continue <- expr({
      break
    })
  } else {
    continue <- expr({
      length(state) <- !!(loop_depth - 1L)
      break
    })
  }

  block <- expr({
    !!!preamble %&&% list(user_call(preamble))
    !!!continue
  })
  state <- new_state(block, NULL, counter())
  counter(inc = 1L)

  state
}

next_state <- function(preamble, counter, info) {
  loop_depth <- loop_depth(counter)

  if (loop_depth == machine_depth(counter)) {
    continue <- expr({
      state[[!!loop_depth]] <- 1L
    })
  } else {
    continue <- expr({
      length(state) <- !!loop_depth
      break
    })
  }

  block <- expr({
    !!!preamble %&&% list(user_call(preamble))
    !!!continue
  })
  state <- new_state(block, NULL, counter())
  counter(inc = 1L)

  state
}

# Must be trailing or before a `next` statement
continue_call <- function(next_i, depth) {
  if (next_i) {
    expr(state[[!!depth]] <- !!next_i)
  } else {
    quote(break)
  }
}

new_state <- function(car, cdr, tag) {
  node <- new_node(car, cdr)

  if (!is_null(tag)) {
    node_poke_tag(node, sym(as.character(tag)))
  }

  node
}

new_counter <- function(machine_depth, loop_depth = 0L) {
  i <- 1L
  function(inc = 0L) {
    i <<- i + inc
    i
  }
}
new_loop_counter <- function(counter) {
  loop_depth <- machine_depth(counter) + 1L
  new_counter(loop_depth, loop_depth = loop_depth)
}

machine_depth <- function(counter) {
  env_get(fn_env(counter), "machine_depth")
}
loop_depth <- function(counter, check = TRUE) {
  depth <- env_get(fn_env(counter), "loop_depth")

  if (check && !depth) {
    abort("Must use `next` and `break` within a loop.")
  }

  depth
}

machine_info <- function(type, env) {
  type <- arg_match0(type, c("generator", "async", "async_generator"))

  if (type %in% c("async", "async_generator")) {
    # Look up lexically defined async operations
    async_ops <- flowery_ops(env)
  } else {
    async_ops <- NULL
  }

  list(
    type = type,
    async_ops = async_ops
  )
}