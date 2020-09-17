# async functions construct a generator

    Code
      async_state_machine(function() "value")
    Output
      $`1`
      {
          flowery::coro_return(`_as_promise`("value"))
      }
      
      $`2`
      {
          base::return(invisible(NULL))
      }
      
      [[3]]
      {
          rlang::abort(base::sprintf("Internal error: Unexpected state `%s`.", 
              `_state`))
      }
      

---

    Code
      async_state_machine(function() await("value"))
    Output
      $`1`
      {
          flowery::coro_yield("2", `_then`(`_as_promise`("value"), 
              callback = `_self`))
      }
      
      $`2`
      {
          flowery::coro_return(`_as_promise`(invisible(NULL)))
      }
      
      $`3`
      {
          base::return(invisible(NULL))
      }
      
      [[4]]
      {
          rlang::abort(base::sprintf("Internal error: Unexpected state `%s`.", 
              `_state`))
      }
      

---

    Code
      async_state_machine(function() if (1) await("value") else "else")
    Output
      $`1`
      if (1) {
          flowery::coro_yield("2", `_then`(`_as_promise`("value"), 
              callback = `_self`))
      } else {
          flowery::coro_return(`_as_promise`("else"))
      }
      
      $`2`
      {
          flowery::coro_return(`_as_promise`(invisible(NULL)))
      }
      
      $`3`
      {
          base::return(invisible(NULL))
      }
      
      [[4]]
      {
          rlang::abort(base::sprintf("Internal error: Unexpected state `%s`.", 
              `_state`))
      }
      

---

    Code
      async_state_machine(function() while (1) if (2) await("value"))
    Output
      $`1`
      {
          if (1) {
              flowery::coro_goto("2")
          }
          else {
              flowery::coro_goto("4")
          }
      }
      
      $`2`
      {
          if (2) {
              flowery::coro_yield("3", `_then`(`_as_promise`("value"), 
                  callback = `_self`))
          }
          flowery::coro_goto("3")
      }
      
      $`3`
      {
          flowery::coro_goto("1")
      }
      
      $`4`
      {
          flowery::coro_return(`_as_promise`(invisible(NULL)))
      }
      
      $`5`
      {
          base::return(invisible(NULL))
      }
      
      [[6]]
      {
          rlang::abort(base::sprintf("Internal error: Unexpected state `%s`.", 
              `_state`))
      }
      

---

    Code
      async_state_machine(function() while (1) foo <- await("value"))
    Output
      $`1`
      {
          if (1) {
              flowery::coro_goto("2")
          }
          else {
              flowery::coro_goto("4")
          }
      }
      
      $`2`
      {
          flowery::coro_yield("3", `_then`(`_as_promise`("value"), 
              callback = `_self`))
      }
      
      $`3`
      {
          foo <- `_next_arg`
          flowery::coro_goto("1")
      }
      
      $`4`
      {
          flowery::coro_return(`_as_promise`(invisible(NULL)))
      }
      
      $`5`
      {
          base::return(invisible(NULL))
      }
      
      [[7]]
      {
          rlang::abort(base::sprintf("Internal error: Unexpected state `%s`.", 
              `_state`))
      }
      

# for loops support await_each()

    Code
      async_state_machine(function(s1, s2) {
        for (x in await_each(s1)) {
          values <<- c(values, x)
          for (y in await_each(s2)) {
            values <<- c(values, y)
          }
        }
      })
    Output
      $`1`
      {
          if (TRUE) {
              flowery::coro_goto("2")
          }
          else {
              flowery::coro_goto("10")
          }
      }
      
      $`2`
      {
          flowery::coro_yield("3", `_then`(`_as_promise`(s1()), callback = `_self`))
      }
      
      $`3`
      {
          x <- `_next_arg`
          if (base::is.null(x)) {
              flowery::coro_goto("10")
          }
          flowery::coro_goto("4")
      }
      
      $`4`
      {
          values <<- c(values, x)
          flowery::coro_goto("5")
      }
      
      $`5`
      {
          if (TRUE) {
              flowery::coro_goto("6")
          }
          else {
              flowery::coro_goto("9")
          }
      }
      
      $`6`
      {
          flowery::coro_yield("7", `_then`(`_as_promise`(s2()), callback = `_self`))
      }
      
      $`7`
      {
          y <- `_next_arg`
          if (base::is.null(y)) {
              flowery::coro_goto("9")
          }
          flowery::coro_goto("8")
      }
      
      $`8`
      {
          values <<- c(values, y)
          flowery::coro_goto("5")
      }
      
      $`9`
      {
          flowery::coro_goto("1")
      }
      
      $`10`
      {
          flowery::coro_return(`_as_promise`(invisible(NULL)))
      }
      
      $`11`
      {
          base::return(invisible(NULL))
      }
      

