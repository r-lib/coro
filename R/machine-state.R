
current_state <- new_environment()

new_machine_parts_state <- function(pause_sym = quote(yield)) {
  list(
    idx = 1L,
    goto = NULL,
    pauses = NULL,
    loop_next = NULL,
    loop_break = NULL,
    has_past = NULL,
    pause_sym = pause_sym,
    coroutine = FALSE
  )
}

peek_state <- function() {
  current_state$idx
}
poke_state <- function(idx = NULL) {
  idx <- idx %||% (current_state$idx + 1L)
  current_state$idx <- idx
  current_state$idx
}
reset_state <- function(state = NULL) {
  state <- state %||% new_machine_parts_state()
  env_bind(current_state, !!!state)
}

local_state <- function(idx, frame = caller_env()) {
  old <- peek_state()
  poke_state(idx)

  restore_state_call <- call2(poke_state, old)
  local_exit(!!restore_state_call, frame = frame)

  invisible(old)
}
with_state <- function(idx, expr) {
  local_state(idx)
  expr
}

peek_state_elt <- function(elt) {
  current_state[[elt]]
}
poke_state_elt <- function(elt, value) {
  old <- current_state[[elt]]
  current_state[[elt]] <- value
  invisible(old)
}
local_state_elt <- function(elt, value, frame = caller_env()) {
  old <- poke_state_elt(elt, value)

  restore_state_call <- call2(poke_state_elt, elt, old)
  local_exit(!!restore_state_call, frame = frame)

  invisible(old)
}
local_state_elts <- function(elts, frame = caller_env()) {
  nms <- names(elts)
  old <- new_list(length(elts))

  exit_call <- block()
  cur <- exit_call

  for (i in seq_along(elts)) {
    old <- poke_state_elt(nms[[i]], elts[[i]])
    old[[i]] <- old %||% list(NULL)

    restore_state_call <- call2(poke_state_elt, nms[[i]], old[[i]])
    node_poke_cdr(cur, pairlist(restore_state_call))
    cur <- node_cdr(cur)
  }

  local_exit(!!exit_call, frame = frame)

  invisible(old)
}

local_jump_nodes <- function(goto, pauses, has_past, frame = caller_env()) {
  local_state_elts(frame = frame, list(
    goto = goto,
    pauses = pauses,
    has_past = has_past
  ))
}
with_jump_nodes <- function(goto, pauses, has_past, expr) {
  local_jump_nodes(goto, pauses, has_past)
  expr
}
with_loop_nodes <- function(pauses, loop_next, loop_break, expr) {
  local_state_elts(list(
    pauses = pauses,
    goto = loop_next,
    loop_next = loop_next,
    loop_break = loop_break
  ))
  expr
}
peek_goto_node <- function() {
  current_state$goto
}
peek_loop_next_node <- function() {
  current_state$loop_next
}
peek_loop_break_node <- function() {
  current_state$loop_break
}

peek_has_past <- function() {
  peek_state_elt("has_past")
}

push_pause_node <- function(node) {
  pauses <- current_state$pauses
  stopifnot(!is_null(pauses))

  if (is_null_node(pauses)) {
    node_poke_car(pauses, node)
  } else {
    node_list_poke_cdr(pauses, pairlist(node))
  }

  invisible(pauses)
}

pauses_push_state <- function(pauses, state) {
  if (is_null_node(pauses)) {
    return(invisible(pauses))
  }

  node_list_walk_car(pauses, pause_poke_state, state)
  invisible(pauses)
}
pause_poke_state <- function(pause, state) {
  pause_call <- node_car(pause)
  node_poke_cadr(pause_call, as.character(state))
  invisible(pause)
}

peek_pause_sym <- function() {
  current_state$pause_sym
}
