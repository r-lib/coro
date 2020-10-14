
generator_body <- function(fn) {
  info <- machine_info("generator", env = caller_env())
  walk_states(body(fn), info = info)
}

async_body <- function(fn) {
  info <- machine_info("async", env = caller_env())
  walk_states(body(fn), info = info)
}

async_generator_body <- function(fn) {
  info <- machine_info("async_generator", env = caller_env())
  walk_states(body(fn), info = info)
}
