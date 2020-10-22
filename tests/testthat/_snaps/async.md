# async functions construct a generator

    Code
      async_body(function() "value")
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user("value")
              exhausted <- TRUE
              return(as_promise(last_value()))
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      async_body(function() await("value"))
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              .last_value <- then(as_promise(user("value")), callback = .self)
              exhausted <- TRUE
              return(as_promise(last_value()))
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      async_body(function() if (1) await("value") else "else")
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              if (user(1)) {
                  state[[1L]] <- 2L
              } else {
                  state[[1L]] <- 3L
              }
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  .last_value <- then(as_promise(user("value")), callback = .self)
                  exhausted <- TRUE
                  return(as_promise(last_value()))
              }, `2` = {
                  break
              })
              n <- length(state)
              if (n < 1L) {
                  break
              }
              if (n == 1L) {
                  state[[1L]] <- 1L
                  next
              }
              length(state) <- 1L
              break
          }, `3` = {
              repeat switch(state[[2L]], `1` = {
                  user("else")
                  exhausted <- TRUE
                  return(as_promise(last_value()))
              }, `2` = {
                  break
              })
              n <- length(state)
              if (n < 1L) {
                  break
              }
              if (n == 1L) {
                  state[[1L]] <- 1L
                  next
              }
              length(state) <- 1L
              break
          }, `4` = {
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      async_body(function() while (1) if (2) await("value"))
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
                  if (user(2)) {
                      state[[2L]] <- 3L
                  } else {
                      state[[2L]] <- 4L
                  }
                  state[[3L]] <- 1L
              }, `3` = {
                  repeat switch(state[[3L]], `1` = {
                      .last_value <- then(as_promise(user("value")), 
                        callback = .self)
                      state[[3L]] <- 2L
                      suspend()
                      return(last_value())
                  }, `2` = {
                      break
                  })
                  n <- length(state)
                  if (n < 2L) {
                      break
                  }
                  if (n == 2L) {
                      state[[2L]] <- 1L
                      next
                  }
                  length(state) <- 2L
                  state[[2L]] <- 1L
              }, `4` = {
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      async_body(function() while (1) foo <- await("value"))
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
                  .last_value <- then(as_promise(user("value")), callback = .self)
                  state[[2L]] <- 3L
                  suspend()
                  return(last_value())
              }, `3` = {
                  user_env[["foo"]] <- arg
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# for loops support await_each()

    Code
      async_body(function(s1, s2) {
        for (x in await_each(s1)) {
          values <<- c(values, x)
          for (y in await_each(s2)) {
            values <<- c(values, y)
          }
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              iterators[[2L]] <- as_iterator(user(s1))
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  .last_value <- then(as_promise({
                      iterator <- iterators[[2L]]
                      iterator()
                  }), callback = .self)
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  without_call_errors(force(arg))
                  state[[2L]] <- 3L
              }, `3` = {
                  if ({
                      if (is_null(arg)) {
                        FALSE
                      } else {
                        user_env[["x"]] <- arg
                        TRUE
                      }
                  }) {
                      state[[2L]] <- 4L
                  } else {
                      break
                  }
              }, `4` = {
                  user({
                      values <<- c(values, x)
                  })
                  iterators[[3L]] <- as_iterator(user(s2))
                  state[[2L]] <- 5L
                  state[[3L]] <- 1L
              }, `5` = {
                  repeat switch(state[[3L]], `1` = {
                      .last_value <- then(as_promise({
                        iterator <- iterators[[3L]]
                        iterator()
                      }), callback = .self)
                      state[[3L]] <- 2L
                      suspend()
                      return(last_value())
                  }, `2` = {
                      without_call_errors(force(arg))
                      state[[3L]] <- 3L
                  }, `3` = {
                      if ({
                        if (is_null(arg)) {
                          FALSE
                        } else {
                          user_env[["y"]] <- arg
                          TRUE
                        }
                      }) {
                        state[[3L]] <- 4L
                      } else {
                        break
                      }
                  }, `4` = {
                      user({
                        values <<- c(values, y)
                      })
                      state[[3L]] <- 1L
                  })
                  iterators[[3L]] <- NULL
                  length(state) <- 2L
                  state[[2L]] <- 1L
              })
              iterators[[2L]] <- NULL
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

