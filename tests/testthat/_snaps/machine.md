# generators have return states

    Code
      generator_body(function() "foo")
    Output
      repeat switch(state[[1]], `1` = {
          user("foo")
          kill()
          return(last_value())
      }, final = {
          return(invisible(NULL))
      })

---

    Code
      generator_body(function() return("foo"))
    Output
      repeat switch(state[[1]], `1` = {
          user("foo")
          kill()
          return(last_value())
      }, final = {
          return(invisible(NULL))
      })

