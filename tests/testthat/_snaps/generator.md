# generator factories print nicely

    Code
      print(factory, reproducible = TRUE)
    Output
      <generator>
      function() yield(NULL)

---

    Code
      print(factory, reproducible = TRUE, internals = TRUE)
    Output
      <generator>
      function() yield(NULL)
      State machine:
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user(NULL)
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) exhausted() else arg
              state[[1L]] <- 3L
          }, `3` = {
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# generator instances prints nicely

    Code
      print(instance, reproducible = TRUE)
    Output
      <generator/instance>
      function() yield(NULL)

---

    Code
      print(instance, reproducible = TRUE, internals = TRUE)
    Output
      <generator/instance>
      function() yield(NULL)
      State machine:
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user(NULL)
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) exhausted() else arg
              state[[1L]] <- 3L
          }, `3` = {
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

