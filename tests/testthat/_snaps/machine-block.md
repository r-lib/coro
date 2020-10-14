# `{` blocks - one pause with no past or future

    Code
      generator_body(function() {
        yield(1L)
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              validate_yield(user({
                  1L
              }))
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `{` blocks - one pause

    Code
      generator_body(function() {
        "before1"
        "before2"
        yield(1L)
        "after1"
        "after2"
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              validate_yield(user({
                  "before1"
                  "before2"
                  1L
              }))
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              user({
                  "after1"
                  "after2"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `{` blocks - no preamble

    Code
      generator_body(function() {
        yield(1L)
        "after"
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              validate_yield(user({
                  1L
              }))
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `{` blocks - multiple pauses

    Code
      generator_body(function() {
        "before"
        yield(1L)
        "during"
        yield(2L)
        "after"
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              validate_yield(user({
                  "before"
                  1L
              }))
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              validate_yield(user({
                  "during"
                  2L
              }))
              state[[1L]] <- 3L
              suspend()
              return(last_value())
          }, `3` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `{` blocks - consecutive pauses

    Code
      generator_body(function() {
        "before"
        yield(1L)
        yield(2L)
        "after"
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              validate_yield(user({
                  "before"
                  1L
              }))
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              validate_yield(user({
                  2L
              }))
              state[[1L]] <- 3L
              suspend()
              return(last_value())
          }, `3` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `{` blocks - return value from pause

    Code
      generator_body(function(x) {
        "before"
        value <- yield(1L)
        "after"
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              validate_yield(user({
                  "before"
                  1L
              }))
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              user_env[["value"]] <- arg
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `{` blocks - no return value

    Code
      generator_body(function() {
        yield(1L)
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              validate_yield(user({
                  1L
              }))
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      generator_body(function() {
        yield(1L)
        yield(2L)
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              validate_yield(user({
                  1L
              }))
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              validate_yield(user({
                  2L
              }))
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `{` blocks - nested

    Code
      generator_body(function() {
        "before1"
        "before2"
        {
          "before-inner"
          yield(1L)
          "after-inner"
        }
        "after1"
        "after2"
      })
    Error <rlang_error>
      TODO in `block_states()`: {

# `{` blocks - nested and no past before pause

    Code
      generator_body(function() {
        {
          "before-inner"
          yield(1L)
          "after-inner"
        }
        "after1"
        "after2"
      })
    Error <rlang_error>
      TODO in `block_states()`: {

# `{` blocks - nested and goto after pause

    Code
      generator_body(function() {
        {
          "before-inner"
          yield(1L)
        }
        "after1"
        "after2"
      })
    Error <rlang_error>
      TODO in `block_states()`: {

# `{` blocks - complex nesting

    Code
      generator_body(function() {
        "before"
        {
          "before-inner"
          yield(1L)
          {
            yield(2L)
            yield(3L)
          }
          "after-inner"
        }
        "after"
      })
    Error <rlang_error>
      TODO in `block_states()`: {

# `{` blocks - simple nesting with various continuation states

    Code
      generator_body(function() {
        {
          {
            yield(1L)
            "after-inner-inner"
          }
        }
        "after"
      })
    Error <rlang_error>
      TODO in `block_states()`: {

---

    Code
      generator_body(function() {
        {
          {
            yield(1L)
          }
          "after-inner"
        }
        "after"
      })
    Error <rlang_error>
      TODO in `block_states()`: {

# yield assignment in a loop

    Code
      generator_body(function() while (1) var <- yield("value"))
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user(1)) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  validate_yield(user("value"))
                  state[[2L]] <- 3L
                  suspend()
                  return(last_value())
              }, `3` = {
                  user_env[["var"]] <- arg
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

