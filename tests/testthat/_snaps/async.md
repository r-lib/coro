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
          if (!rlang::is_installed(c("promises", "later"))) {
              `_resolved` <- `_next_arg`
              flowery::coro_goto("3")
          }
          flowery::coro_goto("4")
      }
      
      $`3`
      {
          rlang::abort("The {later} and {promises} packages must be installed.")
          flowery::coro_goto("4")
      }
      
      $`4`
      {
          flowery::coro_return(`_as_promise`("value"))
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
      async_state_machine(function() await("value"))
    Output
      $`1`
      {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("2")
      }
      
      $`2`
      {
          if (!rlang::is_installed(c("promises", "later"))) {
              `_resolved` <- `_next_arg`
              flowery::coro_goto("3")
          }
          flowery::coro_goto("4")
      }
      
      $`3`
      {
          rlang::abort("The {later} and {promises} packages must be installed.")
          flowery::coro_goto("4")
      }
      
      $`4`
      {
          flowery::coro_yield("5", `_then`(`_as_promise`("value"), 
              callback = `_self`))
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
      {
          if (!rlang::is_installed(c("promises", "later"))) {
              `_resolved` <- `_next_arg`
              flowery::coro_goto("3")
          }
          flowery::coro_goto("4")
      }
      
      $`3`
      {
          rlang::abort("The {later} and {promises} packages must be installed.")
          flowery::coro_goto("4")
      }
      
      $`4`
      if (1) {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("5")
      } else {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("7")
      }
      
      $`5`
      {
          flowery::coro_yield("6", `_then`(`_as_promise`("value"), 
              callback = `_self`))
      }
      
      $`6`
      {
          flowery::coro_return(`_as_promise`(invisible(NULL)))
      }
      
      $`7`
      {
          flowery::coro_return(`_as_promise`("else"))
      }
      
      $`8`
      {
          base::return(invisible(NULL))
      }
      
      [[9]]
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
          `_resolved` <- `_next_arg`
          flowery::coro_goto("2")
      }
      
      $`2`
      {
          if (!rlang::is_installed(c("promises", "later"))) {
              `_resolved` <- `_next_arg`
              flowery::coro_goto("3")
          }
          flowery::coro_goto("4")
      }
      
      $`3`
      {
          rlang::abort("The {later} and {promises} packages must be installed.")
          flowery::coro_goto("4")
      }
      
      $`4`
      {
          if (1) {
              flowery::coro_goto("5")
          }
          else {
              flowery::coro_goto("8")
          }
      }
      
      $`5`
      {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("6")
      }
      
      $`6`
      {
          if (2) {
              `_resolved` <- `_next_arg`
              flowery::coro_goto("7")
          }
          flowery::coro_goto("4")
      }
      
      $`7`
      {
          flowery::coro_yield("4", `_then`(`_as_promise`("value"), 
              callback = `_self`))
      }
      
      $`8`
      {
          flowery::coro_return(`_as_promise`(invisible(NULL)))
      }
      
      $`9`
      {
          base::return(invisible(NULL))
      }
      
      [[10]]
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
          `_resolved` <- `_next_arg`
          flowery::coro_goto("2")
      }
      
      $`2`
      {
          if (!rlang::is_installed(c("promises", "later"))) {
              `_resolved` <- `_next_arg`
              flowery::coro_goto("3")
          }
          flowery::coro_goto("4")
      }
      
      $`3`
      {
          rlang::abort("The {later} and {promises} packages must be installed.")
          flowery::coro_goto("4")
      }
      
      $`4`
      {
          if (1) {
              flowery::coro_goto("5")
          }
          else {
              flowery::coro_goto("8")
          }
      }
      
      $`5`
      {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("6")
      }
      
      $`6`
      {
          flowery::coro_yield("7", `_then`(`_as_promise`("value"), 
              callback = `_self`))
      }
      
      $`7`
      {
          foo <- `_next_arg`
          flowery::coro_goto("4")
      }
      
      $`8`
      {
          flowery::coro_return(`_as_promise`(invisible(NULL)))
      }
      
      $`9`
      {
          base::return(invisible(NULL))
      }
      
      [[10]]
      {
          rlang::abort(base::sprintf("Internal error: Unexpected state `%s`.", 
              `_state`))
      }
      

