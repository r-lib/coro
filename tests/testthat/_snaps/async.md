# async functions construct a generator

    Code
      async_state_machine(function() "value")
    Output
      $`1`
      {
          if (!rlang::is_installed(c("promises", "later"))) {
              rlang::abort("The {later} and {promises} packages must be installed.")
          }
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
          if (!rlang::is_installed(c("promises", "later"))) {
              rlang::abort("The {later} and {promises} packages must be installed.")
          }
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
      {
          if (!rlang::is_installed(c("promises", "later"))) {
              rlang::abort("The {later} and {promises} packages must be installed.")
          }
          if (1) {
              flowery::coro_yield("2", `_then`(`_as_promise`("value"), 
                  callback = `_self`))
          }
          else {
              flowery::coro_return(`_as_promise`("else"))
          }
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
          if (!rlang::is_installed(c("promises", "later"))) {
              rlang::abort("The {later} and {promises} packages must be installed.")
          }
          flowery::coro_goto("2")
      }
      
      $`2`
      {
          if (1) {
              flowery::coro_goto("3")
          }
          else {
              flowery::coro_goto("4")
          }
      }
      
      $`3`
      {
          if (2) {
              flowery::coro_yield("2", `_then`(`_as_promise`("value"), 
                  callback = `_self`))
          }
          flowery::coro_goto("2")
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
          if (!rlang::is_installed(c("promises", "later"))) {
              rlang::abort("The {later} and {promises} packages must be installed.")
          }
          flowery::coro_goto("2")
      }
      
      $`2`
      {
          if (1) {
              flowery::coro_goto("3")
          }
          else {
              flowery::coro_goto("5")
          }
      }
      
      $`3`
      {
          flowery::coro_yield("4", `_then`(`_as_promise`("value"), 
              callback = `_self`))
      }
      
      $`4`
      {
          foo <- `_next_arg`
          flowery::coro_goto("2")
      }
      
      $`5`
      {
          flowery::coro_return(`_as_promise`(invisible(NULL)))
      }
      
      $`6`
      {
          base::return(invisible(NULL))
      }
      
      [[7]]
      {
          rlang::abort(base::sprintf("Internal error: Unexpected state `%s`.", 
              `_state`))
      }
      

