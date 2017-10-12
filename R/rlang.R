
dots_node <- function(...) {
  node_cdr(substitute(lang(...)))
}

is_language <- is_lang
get_environment <- get_env

node_poke_car <- mut_node_car
node_poke_cdr <- mut_node_cdr
node_poke_tag <- mut_node_tag
node_poke_cadr <- mut_node_cadr
node_poke_cddr <- mut_node_cddr

env_poke <- env_set

poke_attr <- function(x, name, value) {
  invisible(.Call(rlang_poke_attr, x, sym(name), value))
}

scoped_exit <- function(expr, frame = caller_env()) {
  expr <- enexpr(expr)

  # We are at top-level when only one frame refers to the global environment
  if (is_reference(frame, global_env())) {
    is_global_frame <- sys.parents() == 0
    if (sum(is_global_frame) == 1) {
      abort("Can't add an exit event at top-level")
    }
  }

  # Inline everything so the call will succeed in any environment
  expr <- lang(on.exit, expr, add = TRUE)
  eval_bare(expr, frame)

  invisible(expr)
}
