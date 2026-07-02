# setup() compiles to a do_setup() state

    Code
      generator_body(function() {
        setup({
          old <- the$x
          on.exit(the$x <- old, add = TRUE)
        })
        yield(1)
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat {
              switch(state[[1L]], `1` = {
                  do_setup(1L, quote({
                      old <- the$x
                      on.exit(the$x <- old, add = TRUE)
                  }))
                  state[[1L]] <- 2L
              }, `2` = {
                  user({
                      1
                  })
                  state[[1L]] <- 3L
                  suspend()
                  return(last_value())
              }, `3` = {
                  .last_value <- if (missing(arg)) exhausted() else arg
                  state[[1L]] <- 4L
              }, `4` = {
                  exhausted <- TRUE
                  return(exhausted())
              })
          }
          exhausted <- TRUE
          invisible(exhausted())
      }

