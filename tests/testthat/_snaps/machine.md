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

# generators have yield states

    Code
      generator_body(function() yield("foo"))
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
      generator_body(function() flowery::yield("foo"))
    Output
      repeat switch(state[[1]], `1` = {
          user("foo")
          kill()
          return(last_value())
      }, final = {
          return(invisible(NULL))
      })

# generators support blocks

    Code
      generator_body(function() {
        "foo"
        "bar"
      })
    Output
      repeat switch(state[[1]], `1` = {
          user({
              "foo"
              "bar"
          })
          kill()
          return(last_value())
      }, final = {
          return(invisible(NULL))
      })

---

    Code
      generator_body(function() {
        "foo"
        yield("value")
      })
    Output
      repeat switch(state[[1]], `1` = {
          user({
              "foo"
              "value"
          })
          kill()
          return(last_value())
      }, final = {
          return(invisible(NULL))
      })

---

    Code
      generator_body(function() {
        "foo"
        yield("value")
        "bar"
      })
    Output
      repeat switch(state[[1]], `1` = {
          user({
              "foo"
              "value"
          })
          suspend_to(2L)
          return(last_value())
      }, `2` = {
          user({
              "bar"
          })
          kill()
          return(last_value())
      }, final = {
          return(invisible(NULL))
      })

