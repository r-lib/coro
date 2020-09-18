# `{` blocks - one pause with no past or future

    Code
      machine_parts(function() {
        yield(1L)
      })
    Output
      [[1]]
      {
          flowery::coro_yield("2", 1L)
      }
      
      [[2]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

# `{` blocks - one pause

    Code
      machine_parts(function() {
        "before1"
        "before2"
        yield(1L)
        "after1"
        "after2"
      })
    Output
      [[1]]
      {
          "before1"
          "before2"
          flowery::coro_yield("2", 1L)
      }
      
      [[2]]
      {
          "after1"
          flowery::coro_return("after2")
      }
      

# `{` blocks - no preamble

    Code
      machine_parts(function() {
        yield(1L)
        "after"
      })
    Output
      [[1]]
      {
          flowery::coro_yield("2", 1L)
      }
      
      [[2]]
      {
          flowery::coro_return("after")
      }
      

# `{` blocks - multiple pauses

    Code
      machine_parts(function() {
        "before"
        yield(1L)
        "during"
        yield(2L)
        "after"
      })
    Output
      [[1]]
      {
          "before"
          flowery::coro_yield("2", 1L)
      }
      
      [[2]]
      {
          "during"
          flowery::coro_yield("3", 2L)
      }
      
      [[3]]
      {
          flowery::coro_return("after")
      }
      

# `{` blocks - consecutive pauses

    Code
      machine_parts(function() {
        "before"
        yield(1L)
        yield(2L)
        "after"
      })
    Output
      [[1]]
      {
          "before"
          flowery::coro_yield("2", 1L)
      }
      
      [[2]]
      {
          flowery::coro_yield("3", 2L)
      }
      
      [[3]]
      {
          flowery::coro_return("after")
      }
      

# `{` blocks - return value from pause

    Code
      machine_parts(function(x) {
        "before"
        value <- yield(1L)
        "after"
      })
    Output
      [[1]]
      {
          x <- `_next_arg`
          flowery::coro_goto("2")
      }
      
      [[2]]
      {
          "before"
          flowery::coro_yield("3", 1L)
      }
      
      [[3]]
      {
          value <- `_next_arg`
          flowery::coro_return("after")
      }
      

# `{` blocks - no return value

    Code
      machine_parts(function() {
        yield(1L)
      })
    Output
      [[1]]
      {
          flowery::coro_yield("2", 1L)
      }
      
      [[2]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

---

    Code
      machine_parts(function() {
        yield(1L)
        yield(2L)
      })
    Output
      [[1]]
      {
          flowery::coro_yield("2", 1L)
      }
      
      [[2]]
      {
          flowery::coro_yield("3", 2L)
      }
      
      [[3]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

# `{` blocks - nested

    Code
      machine_parts(function() {
        "before1"
        "before2"
        {
          "before-inner"
          yield(1L)
          "after-inner"
        }
        "after1"
        "after2"
      })
    Output
      [[1]]
      {
          "before1"
          "before2"
          {
              "before-inner"
              flowery::coro_yield("2", 1L)
          }
      }
      
      [[2]]
      {
          "after-inner"
          flowery::coro_goto("3")
      }
      
      [[3]]
      {
          "after1"
          flowery::coro_return("after2")
      }
      

# `{` blocks - nested and no past before pause

    Code
      machine_parts(function() {
        {
          "before-inner"
          yield(1L)
          "after-inner"
        }
        "after1"
        "after2"
      })
    Output
      [[1]]
      {
          "before-inner"
          flowery::coro_yield("2", 1L)
      }
      
      [[2]]
      {
          "after-inner"
          flowery::coro_goto("3")
      }
      
      [[3]]
      {
          "after1"
          flowery::coro_return("after2")
      }
      

# `{` blocks - nested and goto after pause

    Code
      machine_parts(function() {
        {
          "before-inner"
          yield(1L)
        }
        "after1"
        "after2"
      })
    Output
      [[1]]
      {
          "before-inner"
          flowery::coro_yield("2", 1L)
      }
      
      [[2]]
      {
          "after1"
          flowery::coro_return("after2")
      }
      

# `{` blocks - complex nesting

    Code
      machine_parts(function() {
        "before"
        {
          "before-inner"
          yield(1L)
          {
            yield(2L)
            yield(3L)
          }
          "after-inner"
        }
        "after"
      })
    Output
      [[1]]
      {
          "before"
          {
              "before-inner"
              flowery::coro_yield("2", 1L)
          }
      }
      
      [[2]]
      {
          flowery::coro_yield("3", 2L)
      }
      
      [[3]]
      {
          flowery::coro_yield("4", 3L)
      }
      
      [[4]]
      {
          "after-inner"
          flowery::coro_goto("5")
      }
      
      [[5]]
      {
          flowery::coro_return("after")
      }
      

# `{` blocks - simple nesting with various continuation states

    Code
      machine_parts(function() {
        {
          {
            yield(1L)
            "after-inner-inner"
          }
        }
        "after"
      })
    Output
      [[1]]
      {
          flowery::coro_yield("2", 1L)
      }
      
      [[2]]
      {
          "after-inner-inner"
          flowery::coro_goto("3")
      }
      
      [[3]]
      {
          flowery::coro_return("after")
      }
      

---

    Code
      machine_parts(function() {
        {
          {
            yield(1L)
          }
          "after-inner"
        }
        "after"
      })
    Output
      [[1]]
      {
          flowery::coro_yield("2", 1L)
      }
      
      [[2]]
      {
          "after-inner"
          flowery::coro_goto("3")
      }
      
      [[3]]
      {
          flowery::coro_return("after")
      }
      

