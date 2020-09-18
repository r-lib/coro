# `if` blocks - one pause

    Code
      machine_parts(function() {
        "before"
        if (TRUE) {
          "if-before"
          yield(1L)
          "if-after"
        } else {
          FALSE
        }
        "after"
      })
    Output
      [[1]]
      {
          "before"
          if (TRUE) {
              "if-before"
              flowery::coro_yield("2", 1L)
          }
          else {
              FALSE
          }
          flowery::coro_goto("3")
      }
      
      [[2]]
      {
          "if-after"
          flowery::coro_goto("3")
      }
      
      [[3]]
      {
          flowery::coro_return("after")
      }
      

# `else` blocks - one pause

    Code
      machine_parts(function() {
        "before"
        if (FALSE) {
          FALSE
        } else {
          "else-before"
          yield(1L)
          "else-after"
        }
        "after"
      })
    Output
      [[1]]
      {
          "before"
          if (FALSE) {
              FALSE
          }
          else {
              "else-before"
              flowery::coro_yield("2", 1L)
          }
          flowery::coro_goto("3")
      }
      
      [[2]]
      {
          "else-after"
          flowery::coro_goto("3")
      }
      
      [[3]]
      {
          flowery::coro_return("after")
      }
      

# `if` blocks - inner block

    Code
      machine_parts(function() {
        "before"
        if (TRUE) {
          "if-before"
          {
            "inner-before"
            yield(1L)
            "inner-after"
          }
          "if-after"
        } else {
          FALSE
        }
        "after"
      })
    Output
      [[1]]
      {
          "before"
          if (TRUE) {
              "if-before"
              {
                  "inner-before"
                  flowery::coro_yield("2", 1L)
              }
          }
          else {
              FALSE
          }
          flowery::coro_goto("4")
      }
      
      [[2]]
      {
          "inner-after"
          flowery::coro_goto("3")
      }
      
      [[3]]
      {
          "if-after"
          flowery::coro_goto("4")
      }
      
      [[4]]
      {
          flowery::coro_return("after")
      }
      

# `if` blocks - nested

    Code
      machine_parts(function() {
        "before"
        if (TRUE) {
          "if-before"
          if (FALSE) yield(1L)
          "if-after"
        } else {
          "foo"
        }
      })
    Output
      [[1]]
      {
          "before"
          if (TRUE) {
              "if-before"
              if (FALSE) {
                  flowery::coro_yield("2", 1L)
              }
              flowery::coro_goto("2")
          }
          else {
              flowery::coro_return("foo")
          }
      }
      
      [[2]]
      {
          flowery::coro_return("if-after")
      }
      

# `if` blocks - nested and trailing pause

    Code
      machine_parts(function() {
        "before"
        if (TRUE) {
          "if-before"
          if (FALSE) yield(1L)
        } else {
          "foo"
        }
      })
    Output
      [[1]]
      {
          "before"
          if (TRUE) {
              "if-before"
              if (FALSE) {
                  flowery::coro_yield("2", 1L)
              }
              else {
                  flowery::coro_return(invisible(NULL))
              }
          }
          else {
              flowery::coro_return("foo")
          }
      }
      
      [[2]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

# `if` blocks - multiply nested and all trailing

    Code
      machine_parts(function() {
        "before"
        if (TRUE) {
          "if-before"
          if (FALSE) {
            if (FALSE) {
              yield(1L)
              "if-3-after"
            }
            "if-2-after"
          }
        } else {
          FALSE
        }
      })
    Output
      [[1]]
      {
          "before"
          if (TRUE) {
              "if-before"
              if (FALSE) {
                  if (FALSE) {
                      flowery::coro_yield("2", 1L)
                  }
                  flowery::coro_goto("3")
              }
              else {
                  flowery::coro_return(invisible(NULL))
              }
          }
          else {
              flowery::coro_return(FALSE)
          }
      }
      
      [[2]]
      {
          "if-3-after"
          flowery::coro_goto("3")
      }
      
      [[3]]
      {
          flowery::coro_return("if-2-after")
      }
      

# `if`-`else` blocks - trailing

    Code
      machine_parts(function() {
        "before"
        if (TRUE) {
          "if-before"
          yield(1L)
          "if-after"
        } else {
          "else-before"
          yield(2L)
          "else-after"
        }
      })
    Output
      [[1]]
      {
          "before"
          if (TRUE) {
              "if-before"
              flowery::coro_yield("2", 1L)
          }
          else {
              "else-before"
              flowery::coro_yield("3", 2L)
          }
      }
      
      [[2]]
      {
          flowery::coro_return("if-after")
      }
      
      [[3]]
      {
          flowery::coro_return("else-after")
      }
      

# `if`-`else` blocks - non trailing

    Code
      machine_parts(function() {
        "before"
        if (TRUE) {
          "if-before"
          yield(1L)
          "if-after"
        } else {
          "else-before"
          yield(2L)
          "else-after"
        }
        "after"
      })
    Output
      [[1]]
      {
          "before"
          if (TRUE) {
              "if-before"
              flowery::coro_yield("2", 1L)
          }
          else {
              "else-before"
              flowery::coro_yield("3", 2L)
          }
      }
      
      [[2]]
      {
          "if-after"
          flowery::coro_goto("4")
      }
      
      [[3]]
      {
          "else-after"
          flowery::coro_goto("4")
      }
      
      [[4]]
      {
          flowery::coro_return("after")
      }
      

# `if`-`else` blocks - same continuation

    Code
      machine_parts(function() {
        "before"
        if (TRUE) {
          yield(1L)
        } else {
          yield(2L)
        }
        "after"
      })
    Output
      [[1]]
      {
          "before"
          if (TRUE) {
              flowery::coro_yield("2", 1L)
          }
          else {
              flowery::coro_yield("2", 2L)
          }
      }
      
      [[2]]
      {
          flowery::coro_return("after")
      }
      

# `if`-`else` blocks - continuation in `if`

    Code
      machine_parts(function() {
        "before"
        if (TRUE) {
          yield(1L)
          "if-after"
        } else {
          yield(2L)
        }
        "after"
      })
    Output
      [[1]]
      {
          "before"
          if (TRUE) {
              flowery::coro_yield("2", 1L)
          }
          else {
              flowery::coro_yield("3", 2L)
          }
      }
      
      [[2]]
      {
          "if-after"
          flowery::coro_goto("3")
      }
      
      [[3]]
      {
          flowery::coro_return("after")
      }
      

# `if`-`else` blocks - continuation in `else`

    Code
      machine_parts(function() {
        "before"
        if (TRUE) {
          yield(1L)
        } else {
          yield(2L)
          "else-after"
        }
        "after"
      })
    Output
      [[1]]
      {
          "before"
          if (TRUE) {
              flowery::coro_yield("3", 1L)
          }
          else {
              flowery::coro_yield("2", 2L)
          }
      }
      
      [[2]]
      {
          "else-after"
          flowery::coro_goto("3")
      }
      
      [[3]]
      {
          flowery::coro_return("after")
      }
      

# `if` blocks - doubly nested with continuation

    Code
      machine_parts(function() {
        if (TRUE) {
          if (TRUE) {
            yield(1L)
            "if-3-after"
          }
        }
        "after"
      })
    Output
      [[1]]
      {
          if (TRUE) {
              if (TRUE) {
                  flowery::coro_yield("2", 1L)
              }
              flowery::coro_goto("3")
          }
          flowery::coro_goto("3")
      }
      
      [[2]]
      {
          "if-3-after"
          flowery::coro_goto("3")
      }
      
      [[3]]
      {
          flowery::coro_return("after")
      }
      

# `if`-`else` blocks - multiply nested and not trailing

    Code
      machine_parts(function() {
        "before"
        if (TRUE) {
          "if-before"
          if (TRUE) {
            if (TRUE) {
              yield(1L)
              "if-3-after"
            }
            "if-2-after"
          } else {
            if (FALSE) {
              FALSE
            } else {
              yield(2L)
            }
          }
        } else {
          FALSE
        }
        "after"
      })
    Output
      [[1]]
      {
          "before"
          if (TRUE) {
              "if-before"
              if (TRUE) {
                  if (TRUE) {
                      flowery::coro_yield("2", 1L)
                  }
                  flowery::coro_goto("3")
              }
              else {
                  if (FALSE) {
                      FALSE
                  }
                  else {
                      flowery::coro_yield("4", 2L)
                  }
                  flowery::coro_goto("4")
              }
          }
          else {
              FALSE
          }
          flowery::coro_goto("4")
      }
      
      [[2]]
      {
          "if-3-after"
          flowery::coro_goto("3")
      }
      
      [[3]]
      {
          "if-2-after"
          flowery::coro_goto("4")
      }
      
      [[4]]
      {
          flowery::coro_return("after")
      }
      

