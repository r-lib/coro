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
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

