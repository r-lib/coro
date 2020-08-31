
dots_node <- function(...) {
  node_cdr(substitute(call(...)))
}

get_environment <- get_env

poke_attr <- function(x, name, value) {
  invisible(.Call(rlang_poke_attr, x, sym(name), value))
}

local_exit <- function(expr, frame = caller_env()) {
  expr <- enexpr(expr)

  # We are at top-level when only one frame refers to the global environment
  if (is_reference(frame, global_env())) {
    is_global_frame <- sys.parents() == 0
    if (sum(is_global_frame) == 1) {
      abort("Can't add an exit event at top-level")
    }
  }

  # Inline everything so the call will succeed in any environment
  expr <- call2(on.exit, expr, add = TRUE)
  eval_bare(expr, frame)

  invisible(expr)
}

new_function <- function(body, args = list(), env = caller_env()) {
  stopifnot(all(have_name(args)), is_expr(body), is_env(env))
  args <- as_pairlist(args)
  eval_bare(call("function", args, body), env)
}

add_attributes <- function(.x, ...) {
  attributes(.x) <- list2(...)
  .x
}

set_class <- function(x, class) {
  class(x) <- class
  x
}

blast <- function(expr, env = caller_env()) {
  eval_bare(enexpr(expr), env)
}
