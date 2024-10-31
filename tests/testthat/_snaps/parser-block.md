# `{` blocks - one pause with no past or future

    Code
      generator_body(function() {
        yield(1L)
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  1L
              })
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) exhausted() else arg
              state[[1L]] <- 3L
          }, `3` = {
              exhausted <- TRUE
              return(exhausted())
          })
          exhausted <- TRUE
          invisible(exhausted())
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
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before1"
                  "before2"
                  1L
              })
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) NULL else arg
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "after1"
                  "after2"
              })
              exhausted <- TRUE
              return(exhausted())
          })
          exhausted <- TRUE
          invisible(exhausted())
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
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  1L
              })
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) NULL else arg
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(exhausted())
          })
          exhausted <- TRUE
          invisible(exhausted())
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
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
                  1L
              })
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) NULL else arg
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "during"
                  2L
              })
              state[[1L]] <- 4L
              suspend()
              return(last_value())
          }, `4` = {
              .last_value <- if (missing(arg)) NULL else arg
              state[[1L]] <- 5L
          }, `5` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(exhausted())
          })
          exhausted <- TRUE
          invisible(exhausted())
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
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
                  1L
              })
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) NULL else arg
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  2L
              })
              state[[1L]] <- 4L
              suspend()
              return(last_value())
          }, `4` = {
              .last_value <- if (missing(arg)) NULL else arg
              state[[1L]] <- 5L
          }, `5` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(exhausted())
          })
          exhausted <- TRUE
          invisible(exhausted())
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
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
                  1L
              })
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              user_env[["value"]] <- if (missing(arg)) NULL else arg
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(exhausted())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# `{` blocks - no return value

    Code
      generator_body(function() {
        yield(1L)
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  1L
              })
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) exhausted() else arg
              state[[1L]] <- 3L
          }, `3` = {
              exhausted <- TRUE
              return(exhausted())
          })
          exhausted <- TRUE
          invisible(exhausted())
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
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  1L
              })
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) NULL else arg
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  2L
              })
              state[[1L]] <- 4L
              suspend()
              return(last_value())
          }, `4` = {
              .last_value <- if (missing(arg)) exhausted() else arg
              state[[1L]] <- 5L
          }, `5` = {
              exhausted <- TRUE
              return(exhausted())
          })
          exhausted <- TRUE
          invisible(exhausted())
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
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before1"
                  "before2"
                  "before-inner"
                  1L
              })
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) NULL else arg
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "after-inner"
                  "after1"
                  "after2"
              })
              exhausted <- TRUE
              return(exhausted())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

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
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before-inner"
                  1L
              })
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) NULL else arg
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "after-inner"
                  "after1"
                  "after2"
              })
              exhausted <- TRUE
              return(exhausted())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

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
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before-inner"
                  1L
              })
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) NULL else arg
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "after1"
                  "after2"
              })
              exhausted <- TRUE
              return(exhausted())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

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
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
                  "before-inner"
                  1L
              })
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) NULL else arg
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  2L
              })
              state[[1L]] <- 4L
              suspend()
              return(last_value())
          }, `4` = {
              .last_value <- if (missing(arg)) NULL else arg
              state[[1L]] <- 5L
          }, `5` = {
              user({
                  3L
              })
              state[[1L]] <- 6L
              suspend()
              return(last_value())
          }, `6` = {
              .last_value <- if (missing(arg)) NULL else arg
              state[[1L]] <- 7L
          }, `7` = {
              user({
                  "after-inner"
                  "after"
              })
              exhausted <- TRUE
              return(exhausted())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

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
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  1L
              })
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) NULL else arg
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "after-inner-inner"
                  "after"
              })
              exhausted <- TRUE
              return(exhausted())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

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
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  1L
              })
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) NULL else arg
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "after-inner"
                  "after"
              })
              exhausted <- TRUE
              return(exhausted())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# yield assignment in a loop

    Code
      generator_body(function() while (1) var <- yield("value"))
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
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
                  user("value")
                  state[[2L]] <- 3L
                  suspend()
                  return(last_value())
              }, `3` = {
                  user_env[["var"]] <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

