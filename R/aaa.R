#' @import rlang
"_PACKAGE"

.onLoad <- function(...) {
  run_on_load()
}

`%<~%` <- function(lhs, rhs, env = caller_env()) {
  env_bind_lazy(env, !!substitute(lhs) := !!substitute(rhs), .eval_env = env)
}

on_load <- function(expr, env = topenv(caller_env())) {
  callback <- function() eval_bare(expr, env)

  hook <- env$.__rlang_hook__.
  env$.__rlang_hook__. <- list2(!!!hook, callback)
}

run_on_load <- function(env = topenv(caller_env())) {
  hook <- env$.__rlang_hook__.
  env_unbind(env, ".__rlang_hook__.")

  for (callback in hook) {
    callback()
  }

  NULL
}

on_package_load <- function(pkg, expr) {
  fn <- as_function(enquo0(expr))
  setHook(packageEvent(pkg, "onLoad"), fn)

  if (isNamespaceLoaded(pkg)) {
    fn()
  }
}

s3_has_method <- function(generic, class) {
  stopifnot(
    is_string(generic),
    is_string(class)
  )

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  table <- ns_env(package)$.__S3MethodsTable__.

  nm <- paste0(generic, ".", class)
  !is_null(table[[nm]])
}

s3_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  caller <- parent.frame()

  get_method_env <- function() {
    top <- topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    } else {
      caller
    }
  }
  get_method <- function(method, env) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    } else {
      method
    }
  }

  method_fn <- get_method(method)
  stopifnot(is.function(method_fn))

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(package, "onLoad"),
    function(...) {
      ns <- asNamespace(package)

      # Refresh the method, it might have been updated by `devtools::load_all()`
      method_fn <- get_method(method)

      registerS3method(generic, class, method_fn, envir = ns)
    }
  )

  # Avoid registration failures during loading (pkgload or regular)
  if (!isNamespaceLoaded(package)) {
    return(invisible())
  }

  envir <- asNamespace(package)

  # Only register if generic can be accessed
  if (exists(generic, envir)) {
    registerS3method(generic, class, method_fn, envir = envir)
  }

  invisible()
}
