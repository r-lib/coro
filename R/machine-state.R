
state <- new_environment(list(
  idx = 1L,
  goto = NULL,
  pause = NULL
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
with_pause_node <- function(pause, expr) {
  scoped_state_elt("pause", pause)
  expr
}
peek_goto_node <- function() {
  state$goto
}
peek_pause_node <- function() {
  state$pause
}
