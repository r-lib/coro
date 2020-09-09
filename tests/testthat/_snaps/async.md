# async functions construct a generator

    Code
      print(async_generator(function() "value"))
    Output
      [[1]]
      {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("2")
      }
      
      [[2]]
      {
          flowery::coro_return("value")
      }
      

---

    Code
      print(async_generator(function() await("value")))
    Output
      [[1]]
      {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("2")
      }
      
      [[2]]
      {
          flowery::coro_yield("3", promises::then("value", `_async_generator`))
      }
      
      [[3]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

---

    Code
      print(async_generator(function() if (1) await("value") else "else"))
    Output
      [[1]]
      {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("2")
      }
      
      [[2]]
      if (1) {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("3")
      } else {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("5")
      }
      
      [[3]]
      {
          flowery::coro_yield("4", promises::then("value", `_async_generator`))
      }
      
      [[4]]
      {
          flowery::coro_return(invisible(NULL))
      }
      
      [[5]]
      {
          flowery::coro_return("else")
      }
      

---

    Code
      print(async_generator(function() while (1) if (2) await("value")))
    Output
      [[1]]
      {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("2")
      }
      
      [[2]]
      {
          if (1) {
              flowery::coro_goto("3")
          }
          else {
              flowery::coro_goto("6")
          }
      }
      
      [[3]]
      {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("4")
      }
      
      [[4]]
      {
          if (2) {
              `_resolved` <- `_next_arg`
              flowery::coro_goto("5")
          }
          flowery::coro_goto("2")
      }
      
      [[5]]
      {
          flowery::coro_yield("2", promises::then("value", `_async_generator`))
      }
      
      [[6]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

---

    Code
      print(async_generator(function() while (1) foo <- await("value")))
    Output
      [[1]]
      {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("2")
      }
      
      [[2]]
      {
          if (1) {
              flowery::coro_goto("3")
          }
          else {
              flowery::coro_goto("6")
          }
      }
      
      [[3]]
      {
          `_resolved` <- `_next_arg`
          flowery::coro_goto("4")
      }
      
      [[4]]
      {
          flowery::coro_yield("5", promises::then("value", `_async_generator`))
      }
      
      [[5]]
      {
          foo <- `_next_arg`
          flowery::coro_goto("2")
      }
      
      [[6]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

