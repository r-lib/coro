# async functions construct a generator

    Code
      async_state_machine(function() "value")
    Output
      $`1`
      {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("2")
      }
      
      $`2`
      {
          flowery::coro_return(`_as_promise`("value"))
      }
      
      $`3`
      {
          base::return(invisible(NULL))
      }
      

---

    Code
      async_state_machine(function() await("value"))
    Output
      $`1`
      {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("2")
      }
      
      $`2`
      {
          flowery::coro_yield("3", `_then`(`_as_promise`("value"), 
              callback = `_self`))
      }
      
      $`3`
      {
          flowery::coro_return(`_as_promise`(invisible(NULL)))
      }
      
      $`4`
      {
          base::return(invisible(NULL))
      }
      

---

    Code
      async_state_machine(function() if (1) await("value") else "else")
    Output
      $`1`
      {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("2")
      }
      
      $`2`
      if (1) {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("3")
      } else {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("5")
      }
      
      $`3`
      {
          flowery::coro_yield("4", `_then`(`_as_promise`("value"), 
              callback = `_self`))
      }
      
      $`4`
      {
          flowery::coro_return(`_as_promise`(invisible(NULL)))
      }
      
      $`5`
      {
          flowery::coro_return(`_as_promise`("else"))
      }
      
      $`6`
      {
          base::return(invisible(NULL))
      }
      

---

    Code
      async_state_machine(function() while (1) if (2) await("value"))
    Output
      $`1`
      {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("2")
      }
      
      $`2`
      {
          if (1) {
              flowery::coro_goto("3")
          }
          else {
              flowery::coro_goto("6")
          }
      }
      
      $`3`
      {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("4")
      }
      
      $`4`
      {
          if (2) {
              `_resolved` <- `_next_arg`
              flowery::coro_goto("5")
          }
          flowery::coro_goto("2")
      }
      
      $`5`
      {
          flowery::coro_yield("2", `_then`(`_as_promise`("value"), 
              callback = `_self`))
      }
      
      $`6`
      {
          flowery::coro_return(`_as_promise`(invisible(NULL)))
      }
      
      $`7`
      {
          base::return(invisible(NULL))
      }
      

---

    Code
      async_state_machine(function() while (1) foo <- await("value"))
    Output
      $`1`
      {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("2")
      }
      
      $`2`
      {
          if (1) {
              flowery::coro_goto("3")
          }
          else {
              flowery::coro_goto("6")
          }
      }
      
      $`3`
      {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("4")
      }
      
      $`4`
      {
          flowery::coro_yield("5", `_then`(`_as_promise`("value"), 
              callback = `_self`))
      }
      
      $`5`
      {
          foo <- `_next_arg`
          flowery::coro_goto("2")
      }
      
      $`6`
      {
          flowery::coro_return(`_as_promise`(invisible(NULL)))
      }
      
      $`7`
      {
          base::return(invisible(NULL))
      }
      

