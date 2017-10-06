
state <- new_environment(list(
  idx = 1L,
  goto = NULL,
  pause = NULL,
  loop_next = NULL,
  loop_break = NULL
))

peek_state <- function() {
  state$idx
}
poke_state <- function(idx = NULL) {
  idx <- idx %||% (state$idx + 1L)
  state$idx <- idx
  state$idx
}
reset_state <- function() {
  state$idx <- 1L
}

peek_state_elt <- function(elt) {
  state[[elt]]
}
poke_state_elt <- function(elt, value) {
  old <- state[[elt]]
  state[[elt]] <- value
  invisible(old)
}
scoped_state_elt <- function(elt, value, frame = caller_env()) {
  old <- poke_state_elt(elt, value)

  restore_state_lang <- lang(poke_state_elt, elt, old)
  scoped_exit(!! restore_state_lang, frame = frame)

  invisible(old)
}

with_jump_nodes <- function(goto, pause, expr) {
  scoped_state_elt("goto", goto)
  scoped_state_elt("pause", pause)
  expr
}
with_loop_nodes <- function(pause, loop_next, loop_break, expr) {
  scoped_state_elt("pause", pause)
  scoped_state_elt("loop_next", loop_next)
  scoped_state_elt("loop_break", loop_break)
  expr
}
peek_goto_node <- function() {
  state$goto
}
peek_pause_node <- function() {
  state$pause
}
peek_loop_next_node <- function() {
  state$loop_next
}
peek_loop_break_node <- function() {
  state$loop_break
}
