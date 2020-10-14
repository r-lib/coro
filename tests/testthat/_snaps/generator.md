# generator prints nicely

    Code
      print(zap_env(gen({
        while (TRUE) {
          if (TRUE) {
            yield(1)
          }
          return(2)
        }
      })))
    Output
      function (arg = NULL) 
      {
          delayedAssign("arg", arg, assign.env = env)
          evalq(envir = env, {
              if (exhausted) {
                  return(invisible(NULL))
              }
              repeat switch(state[[1L]], `1` = {
                  state[[1L]] <- 2L
                  state[[2L]] <- 1L
              }, `2` = {
                  repeat switch(state[[2L]], `1` = {
                      if (user({
                        TRUE
                      })) {
                        state[[2L]] <- 2L
                      } else {
                        break
                      }
                  }, `2` = {
                      if (user({
                        TRUE
                      })) {
                        state[[2L]] <- 3L
                      } else {
                        state[[2L]] <- 4L
                      }
                      state[[3L]] <- 1L
                  }, `3` = {
                      repeat switch(state[[3L]], `1` = {
                        validate_yield(user({
                          1
                        }))
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
                      state[[2L]] <- 4L
                  }, `4` = {
                      user({
                        2
                      })
                      exhausted <- TRUE
                      return(last_value())
                  })
                  length(state) <- 1L
                  break
              })
              exhausted <- TRUE
              invisible(NULL)
          })
      }

