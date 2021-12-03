wait_for <- function(expr) {
  loop <- later::create_loop(parent = NULL)

  # FIXME: Early exits don't cancel tasks. Important to cancel because
  # there should't be any side effects from the top level event loop
  # after an interruption, error, Q jump, etc.
  on.exit(later::destroy_loop(loop))

  prom <- later::with_loop(loop, expr)

  while (prom_is_pending(prom)) {
    later::run_now(timeoutSecs = Inf, loop = loop)
  }

  prom_value(prom)
}

prom_is_pending <- function(prom) {
  identical(prom_status(prom), "pending")
}
prom_status <- function(prom) {
  impl <- prom_impl(prom)
  impl$status()
}

# FIXME: Is there a better way to dereference resolved promises?
prom_value <- function(prom) {
  impl <- prom_impl(prom)
  out <- .subset2(impl, ".__enclos_env__")$private$value

  if (is_string(prom_status(prom), "rejected")) {
    stop(out)
  } else {
    out
  }
}

prom_impl <- function(prom) {
  stopifnot(promises::is.promise(prom))
  attr(prom, "promise_impl", exact = TRUE)
}
