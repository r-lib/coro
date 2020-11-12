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
              if (missing(arg)) {
                  .last_value <- exhausted()
              } else {
                  .last_value <- without_call_errors(force(arg))
              }
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
              if (missing(arg)) {
                  .last_value <- exhausted()
              } else {
                  .last_value <- without_call_errors(force(arg))
              }
              state[[1L]] <- 3L
          }, `3` = {
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

