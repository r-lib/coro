# `repeat` - one pause

    Code
      generator_body(function() {
        "before"
        repeat {
          "loop-before"
          yield(1L)
          "loop-after"
        }
        "after"
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  validate_yield(user({
                      "loop-before"
                      1L
                  }))
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  user({
                      "loop-after"
                  })
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `repeat` - no continuation

    Code
      generator_body(function() {
        "before"
        repeat yield(1L)
        "after"
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  validate_yield(user(1L))
                  state[[2L]] <- 1L
                  suspend()
                  return(last_value())
              })
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      generator_body(function() {
        "before"
        repeat {
          {
            yield(1L)
          }
        }
        "after"
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  validate_yield(user({
                      1L
                  }))
                  state[[2L]] <- 1L
                  suspend()
                  return(last_value())
              })
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `repeat` - pause within `if`

    Code
      generator_body(function() {
        "before"
        repeat {
          "loop-before"
          if (TRUE) yield(1L)
          "loop-after"
        }
        "after"
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "loop-before"
                  })
                  if (user({
                      TRUE
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      state[[2L]] <- 3L
                  }
                  state[[3L]] <- 1L
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      validate_yield(user(1L))
                      state[[3L]] <- 2L
                      suspend()
                      return(last_value())
                  }, `2` = {
                      break
                  })
                  n <- length(state)
                  if (n < 2L) {
                      break
                  }
                  if (n == 2L) {
                      state[[2L]] <- 1L
                      next
                  }
                  length(state) <- 2L
                  state[[2L]] <- 3L
              }, `3` = {
                  user({
                      "loop-after"
                  })
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `repeat` - nested loop

    Code
      generator_body(function() {
        "before"
        repeat {
          "loop-before"
          repeat yield(1L)
          "loop-after"
        }
        "after"
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "loop-before"
                      "repeat"
                  })
                  state[[2L]] <- 2L
                  state[[3L]] <- 1L
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      validate_yield(user(1L))
                      state[[3L]] <- 1L
                      suspend()
                      return(last_value())
                  })
                  length(state) <- 2L
                  state[[2L]] <- 3L
              }, `3` = {
                  user({
                      "loop-after"
                  })
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `repeat` - non-yielding

    Code
      generator_body(function() {
        "before"
        repeat NULL
        yield(1L)
        "after"
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user(NULL)
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              validate_yield(user({
                  1L
              }))
              state[[1L]] <- 4L
              suspend()
              return(last_value())
          }, `4` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `repeat` - non-yielding but other control flow constructs

    Code
      generator_body(function() {
        "before"
        repeat if (TRUE) break else next
        yield(1L)
        "after"
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user(TRUE)) {
                      state[[2L]] <- 2L
                  } else {
                      state[[2L]] <- 3L
                  }
                  state[[3L]] <- 1L
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      length(state) <- 1L
                      break
                  }, `2` = {
                      break
                  })
                  n <- length(state)
                  if (n < 2L) {
                      break
                  }
                  if (n == 2L) {
                      state[[2L]] <- 1L
                      next
                  }
                  length(state) <- 2L
                  state[[2L]] <- 1L
              }, `3` = {
                  repeat switch(state[[3L]], `1` = {
                      length(state) <- 2L
                      break
                  }, `2` = {
                      break
                  })
                  n <- length(state)
                  if (n < 2L) {
                      break
                  }
                  if (n == 2L) {
                      state[[2L]] <- 1L
                      next
                  }
                  length(state) <- 2L
                  state[[2L]] <- 1L
              }, `4` = {
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              validate_yield(user({
                  1L
              }))
              state[[1L]] <- 4L
              suspend()
              return(last_value())
          }, `4` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# loops - single `next`

    Code
      generator_body(function() {
        repeat {
          next
          yield(1L)
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "next"
                  })
                  state[[2L]] <- 1L
              }, `2` = {
                  validate_yield(user({
                      1L
                  }))
                  state[[2L]] <- 1L
                  suspend()
                  return(last_value())
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# loops - single `next` with past and future

    Code
      generator_body(function() {
        repeat {
          "loop-before"
          yield(1L)
          "loop-after"
          next
          "next-after"
          yield(2L)
          "loop-final"
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  validate_yield(user({
                      "loop-before"
                      1L
                  }))
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  user({
                      "loop-after"
                      "next"
                  })
                  state[[2L]] <- 1L
              }, `3` = {
                  validate_yield(user({
                      "next-after"
                      2L
                  }))
                  state[[2L]] <- 4L
                  suspend()
                  return(last_value())
              }, `4` = {
                  user({
                      "loop-final"
                  })
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# loops - single `break`

    Code
      generator_body(function() {
        repeat {
          break
          yield(1L)
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "break"
                  })
                  break
              }, `2` = {
                  validate_yield(user({
                      1L
                  }))
                  state[[2L]] <- 1L
                  suspend()
                  return(last_value())
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# loops - `next` and `break` within `if`-`else`

    Code
      generator_body(function() {
        repeat {
          "loop-after"
          if (TRUE) break else next
          "next-after"
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "loop-after"
                  })
                  if (user({
                      TRUE
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      state[[2L]] <- 3L
                  }
                  state[[3L]] <- 1L
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      length(state) <- 1L
                      break
                  }, `2` = {
                      break
                  })
                  n <- length(state)
                  if (n < 2L) {
                      break
                  }
                  if (n == 2L) {
                      state[[2L]] <- 1L
                      next
                  }
                  length(state) <- 2L
                  state[[2L]] <- 4L
              }, `3` = {
                  repeat switch(state[[3L]], `1` = {
                      length(state) <- 2L
                      break
                  }, `2` = {
                      break
                  })
                  n <- length(state)
                  if (n < 2L) {
                      break
                  }
                  if (n == 2L) {
                      state[[2L]] <- 1L
                      next
                  }
                  length(state) <- 2L
                  state[[2L]] <- 4L
              }, `4` = {
                  user({
                      "next-after"
                  })
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# loops - `break` and `next` with past and future

    Code
      generator_body(function() {
        repeat {
          "loop-before"
          yield(1L)
          "loop-after"
          break
          "break-after"
          next
          "next-after"
          yield(2L)
          "loop-final"
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  validate_yield(user({
                      "loop-before"
                      1L
                  }))
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  user({
                      "loop-after"
                      "break"
                  })
                  break
              }, `3` = {
                  user({
                      "break-after"
                      "next"
                  })
                  state[[2L]] <- 1L
              }, `4` = {
                  validate_yield(user({
                      "next-after"
                      2L
                  }))
                  state[[2L]] <- 5L
                  suspend()
                  return(last_value())
              }, `5` = {
                  user({
                      "loop-final"
                  })
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# loops - goto loop start after `if` or `else`

    Code
      generator_body(function() {
        repeat if (TRUE) yield()
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user(TRUE)) {
                      state[[2L]] <- 2L
                  } else {
                      state[[2L]] <- 3L
                  }
                  state[[3L]] <- 1L
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      validate_yield(user(NULL))
                      state[[3L]] <- 2L
                      suspend()
                      return(last_value())
                  }, `2` = {
                      break
                  })
                  n <- length(state)
                  if (n < 2L) {
                      break
                  }
                  if (n == 2L) {
                      state[[2L]] <- 1L
                      next
                  }
                  length(state) <- 2L
                  state[[2L]] <- 1L
              }, `3` = {
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      generator_body(function() {
        repeat if (TRUE) yield(1L) else FALSE
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user(TRUE)) {
                      state[[2L]] <- 2L
                  } else {
                      state[[2L]] <- 3L
                  }
                  state[[3L]] <- 1L
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      validate_yield(user(1L))
                      state[[3L]] <- 2L
                      suspend()
                      return(last_value())
                  }, `2` = {
                      break
                  })
                  n <- length(state)
                  if (n < 2L) {
                      break
                  }
                  if (n == 2L) {
                      state[[2L]] <- 1L
                      next
                  }
                  length(state) <- 2L
                  state[[2L]] <- 1L
              }, `3` = {
                  repeat switch(state[[3L]], `1` = {
                      user(FALSE)
                      state[[3L]] <- 2L
                  }, `2` = {
                      break
                  })
                  n <- length(state)
                  if (n < 2L) {
                      break
                  }
                  if (n == 2L) {
                      state[[2L]] <- 1L
                      next
                  }
                  length(state) <- 2L
                  state[[2L]] <- 1L
              }, `4` = {
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `while` - single pause no past or future

    Code
      generator_body(function() {
        while (TRUE) yield(1L)
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      TRUE
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  validate_yield(user(1L))
                  state[[2L]] <- 1L
                  suspend()
                  return(last_value())
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `while` - pause within `if`

    Code
      generator_body(function() {
        while (TRUE) {
          if (FALSE) yield(1L)
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      TRUE
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  if (user({
                      FALSE
                  })) {
                      state[[2L]] <- 3L
                  } else {
                      state[[2L]] <- 4L
                  }
                  state[[3L]] <- 1L
              }, `3` = {
                  repeat switch(state[[3L]], `1` = {
                      validate_yield(user(1L))
                      state[[3L]] <- 2L
                      suspend()
                      return(last_value())
                  }, `2` = {
                      break
                  })
                  n <- length(state)
                  if (n < 2L) {
                      break
                  }
                  if (n == 2L) {
                      state[[2L]] <- 1L
                      next
                  }
                  length(state) <- 2L
                  state[[2L]] <- 1L
              }, `4` = {
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `while` - pause within `if` with future

    Code
      generator_body(function() {
        while (TRUE) {
          if (FALSE) {
            yield(1L)
            "after-pause"
          }
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      TRUE
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  if (user({
                      FALSE
                  })) {
                      state[[2L]] <- 3L
                  } else {
                      state[[2L]] <- 4L
                  }
                  state[[3L]] <- 1L
              }, `3` = {
                  repeat switch(state[[3L]], `1` = {
                      validate_yield(user({
                        1L
                      }))
                      state[[3L]] <- 2L
                      suspend()
                      return(last_value())
                  }, `2` = {
                      user({
                        "after-pause"
                      })
                      state[[3L]] <- 3L
                  }, `3` = {
                      break
                  })
                  n <- length(state)
                  if (n < 2L) {
                      break
                  }
                  if (n == 2L) {
                      state[[2L]] <- 1L
                      next
                  }
                  length(state) <- 2L
                  state[[2L]] <- 1L
              }, `4` = {
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `while` - past before loop

    Code
      generator_body(function() {
        "before"
        while (TRUE) {
          "loop-before"
          yield(1L)
          "loop-after"
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      TRUE
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  validate_yield(user({
                      "loop-before"
                      1L
                  }))
                  state[[2L]] <- 3L
                  suspend()
                  return(last_value())
              }, `3` = {
                  user({
                      "loop-after"
                  })
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `while` - pause after loop

    Code
      generator_body(function() {
        while (TRUE) {
          yield(1L)
          "loop-after"
        }
        yield(2L)
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      TRUE
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  validate_yield(user({
                      1L
                  }))
                  state[[2L]] <- 3L
                  suspend()
                  return(last_value())
              }, `3` = {
                  user({
                      "loop-after"
                  })
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              validate_yield(user({
                  2L
              }))
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `while` - complex control flow

    Code
      generator_body(function() {
        "before"
        while (TRUE) break
        while (TRUE) {
          "loop-before"
          yield(1L)
          "loop-after"
          if (TRUE) {
            "break-before"
            break
            "break-after"
          } else {
            "yield-2-before"
            yield(2L)
            "yield-2-after"
          }
          "next-before"
          next
          "loop-end"
        }
        "after"
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      TRUE
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  break
              })
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              state[[1L]] <- 4L
              state[[2L]] <- 1L
          }, `4` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      TRUE
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  validate_yield(user({
                      "loop-before"
                      1L
                  }))
                  state[[2L]] <- 3L
                  suspend()
                  return(last_value())
              }, `3` = {
                  user({
                      "loop-after"
                  })
                  if (user({
                      TRUE
                  })) {
                      state[[2L]] <- 4L
                  } else {
                      state[[2L]] <- 5L
                  }
                  state[[3L]] <- 1L
              }, `4` = {
                  repeat switch(state[[3L]], `1` = {
                      user({
                        "break-before"
                        "break"
                      })
                      length(state) <- 1L
                      break
                  }, `2` = {
                      user({
                        "break-after"
                      })
                      state[[3L]] <- 3L
                  }, `3` = {
                      break
                  })
                  n <- length(state)
                  if (n < 2L) {
                      break
                  }
                  if (n == 2L) {
                      state[[2L]] <- 1L
                      next
                  }
                  length(state) <- 2L
                  state[[2L]] <- 6L
              }, `5` = {
                  repeat switch(state[[3L]], `1` = {
                      validate_yield(user({
                        "yield-2-before"
                        2L
                      }))
                      state[[3L]] <- 2L
                      suspend()
                      return(last_value())
                  }, `2` = {
                      user({
                        "yield-2-after"
                      })
                      state[[3L]] <- 3L
                  }, `3` = {
                      break
                  })
                  n <- length(state)
                  if (n < 2L) {
                      break
                  }
                  if (n == 2L) {
                      state[[2L]] <- 1L
                      next
                  }
                  length(state) <- 2L
                  state[[2L]] <- 6L
              }, `6` = {
                  user({
                      "next-before"
                      "next"
                  })
                  state[[2L]] <- 1L
              }, `7` = {
                  user({
                      "loop-end"
                  })
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              state[[1L]] <- 5L
          }, `5` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `while` - top level break

    Code
      generator_body(function() {
        while (TRUE) {
          "before-break"
          break
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      TRUE
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  user({
                      "before-break"
                      "break"
                  })
                  break
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `for` - top level break (#7)

    Code
      generator_body(function() {
        for (i in x) break
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              iterators[[2L]] <- as_iterator(user(x))
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if ({
                      iterator <- iterators[[2L]]
                      if (is_null(elt <- iterator())) {
                        FALSE
                      } else {
                        user_env[["i"]] <- elt
                        TRUE
                      }
                  }) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  break
              })
              iterators[[2L]] <- NULL
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `for` - one pause with no past or future

    Code
      generator_body(function() {
        for (i in x) yield(1L)
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              iterators[[2L]] <- as_iterator(user(x))
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if ({
                      iterator <- iterators[[2L]]
                      if (is_null(elt <- iterator())) {
                        FALSE
                      } else {
                        user_env[["i"]] <- elt
                        TRUE
                      }
                  }) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  validate_yield(user(1L))
                  state[[2L]] <- 1L
                  suspend()
                  return(last_value())
              })
              iterators[[2L]] <- NULL
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `for` - one pause with past and future

    Code
      generator_body(function() {
        "before"
        for (i in x) {
          "for-before"
          yield(1L)
          "for-after"
        }
        "after"
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
              })
              iterators[[2L]] <- as_iterator(user(x))
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if ({
                      iterator <- iterators[[2L]]
                      if (is_null(elt <- iterator())) {
                        FALSE
                      } else {
                        user_env[["i"]] <- elt
                        TRUE
                      }
                  }) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  validate_yield(user({
                      "for-before"
                      1L
                  }))
                  state[[2L]] <- 3L
                  suspend()
                  return(last_value())
              }, `3` = {
                  user({
                      "for-after"
                  })
                  state[[2L]] <- 1L
              })
              iterators[[2L]] <- NULL
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `for` - one pause within `if` and one `break` within `else`

    Code
      generator_body(function() {
        for (i in x) {
          "for-before"
          if (TRUE) yield(1L) else break
          "if-after"
          next
          "for-after"
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              iterators[[2L]] <- as_iterator(user(x))
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if ({
                      iterator <- iterators[[2L]]
                      if (is_null(elt <- iterator())) {
                        FALSE
                      } else {
                        user_env[["i"]] <- elt
                        TRUE
                      }
                  }) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  user({
                      "for-before"
                  })
                  if (user({
                      TRUE
                  })) {
                      state[[2L]] <- 3L
                  } else {
                      state[[2L]] <- 4L
                  }
                  state[[3L]] <- 1L
              }, `3` = {
                  repeat switch(state[[3L]], `1` = {
                      validate_yield(user(1L))
                      state[[3L]] <- 2L
                      suspend()
                      return(last_value())
                  }, `2` = {
                      break
                  })
                  n <- length(state)
                  if (n < 2L) {
                      break
                  }
                  if (n == 2L) {
                      state[[2L]] <- 1L
                      next
                  }
                  length(state) <- 2L
                  state[[2L]] <- 5L
              }, `4` = {
                  repeat switch(state[[3L]], `1` = {
                      length(state) <- 1L
                      break
                  }, `2` = {
                      break
                  })
                  n <- length(state)
                  if (n < 2L) {
                      break
                  }
                  if (n == 2L) {
                      state[[2L]] <- 1L
                      next
                  }
                  length(state) <- 2L
                  state[[2L]] <- 5L
              }, `5` = {
                  user({
                      "if-after"
                      "next"
                  })
                  state[[2L]] <- 1L
              }, `6` = {
                  user({
                      "for-after"
                  })
                  state[[2L]] <- 1L
              })
              iterators[[2L]] <- NULL
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# `return()` deep in control flow

    Code
      generator_body(function() {
        while (TRUE) if (TRUE) return(1L) else yield(2L)
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      TRUE
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  if (user(TRUE)) {
                      state[[2L]] <- 3L
                  } else {
                      state[[2L]] <- 4L
                  }
                  state[[3L]] <- 1L
              }, `3` = {
                  repeat switch(state[[3L]], `1` = {
                      user(1L)
                      exhausted <- TRUE
                      return(last_value())
                  }, `2` = {
                      break
                  })
                  n <- length(state)
                  if (n < 2L) {
                      break
                  }
                  if (n == 2L) {
                      state[[2L]] <- 1L
                      next
                  }
                  length(state) <- 2L
                  state[[2L]] <- 1L
              }, `4` = {
                  repeat switch(state[[3L]], `1` = {
                      validate_yield(user(2L))
                      state[[3L]] <- 2L
                      suspend()
                      return(last_value())
                  }, `2` = {
                      break
                  })
                  n <- length(state)
                  if (n < 2L) {
                      break
                  }
                  if (n == 2L) {
                      state[[2L]] <- 1L
                      next
                  }
                  length(state) <- 2L
                  state[[2L]] <- 1L
              }, `5` = {
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# nested loops break to outer loops

    Code
      generator_body(function() {
        "before"
        while (TRUE) {
          "while before if"
          if (i > 3) {
            "while-if before break"
            break
          }
          "while after if"
          while (TRUE) {
            "while-while before if"
            if (j > 3) {
              "while-while-if before break"
              break
            }
            "while-while after if"
          }
          "while after while"
        }
        "after"
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      TRUE
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  user({
                      "while before if"
                  })
                  if (user({
                      i > 3
                  })) {
                      state[[2L]] <- 3L
                  } else {
                      state[[2L]] <- 4L
                  }
                  state[[3L]] <- 1L
              }, `3` = {
                  repeat switch(state[[3L]], `1` = {
                      user({
                        "while-if before break"
                        "break"
                      })
                      length(state) <- 1L
                      break
                  }, `2` = {
                      break
                  })
                  n <- length(state)
                  if (n < 2L) {
                      break
                  }
                  if (n == 2L) {
                      state[[2L]] <- 1L
                      next
                  }
                  length(state) <- 2L
                  state[[2L]] <- 4L
              }, `4` = {
                  user({
                      "while after if"
                  })
                  state[[2L]] <- 5L
                  state[[3L]] <- 1L
              }, `5` = {
                  repeat switch(state[[3L]], `1` = {
                      if (user({
                        TRUE
                      })) {
                        state[[3L]] <- 2L
                      } else {
                        break
                      }
                  }, `2` = {
                      user({
                        "while-while before if"
                      })
                      if (user({
                        j > 3
                      })) {
                        state[[3L]] <- 3L
                      } else {
                        state[[3L]] <- 4L
                      }
                      state[[4L]] <- 1L
                  }, `3` = {
                      repeat switch(state[[4L]], `1` = {
                        user({
                          "while-while-if before break"
                          "break"
                        })
                        length(state) <- 2L
                        break
                      }, `2` = {
                        break
                      })
                      n <- length(state)
                      if (n < 3L) {
                        break
                      }
                      if (n == 3L) {
                        state[[3L]] <- 1L
                        next
                      }
                      length(state) <- 3L
                      state[[3L]] <- 4L
                  }, `4` = {
                      user({
                        "while-while after if"
                      })
                      state[[3L]] <- 1L
                  })
                  length(state) <- 2L
                  state[[2L]] <- 6L
              }, `6` = {
                  user({
                      "while after while"
                  })
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      generator_body(function() {
        "before"
        while (TRUE) {
          "while before if"
          if (i > 3) {
            "while-if before break"
            break
          }
          "while after if"
          while (TRUE) {
            "while-while before if"
            if (j > 3) {
              "while-while-if before break"
              break
            }
            "while-while after if"
          }
        }
        "after"
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      TRUE
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  user({
                      "while before if"
                  })
                  if (user({
                      i > 3
                  })) {
                      state[[2L]] <- 3L
                  } else {
                      state[[2L]] <- 4L
                  }
                  state[[3L]] <- 1L
              }, `3` = {
                  repeat switch(state[[3L]], `1` = {
                      user({
                        "while-if before break"
                        "break"
                      })
                      length(state) <- 1L
                      break
                  }, `2` = {
                      break
                  })
                  n <- length(state)
                  if (n < 2L) {
                      break
                  }
                  if (n == 2L) {
                      state[[2L]] <- 1L
                      next
                  }
                  length(state) <- 2L
                  state[[2L]] <- 4L
              }, `4` = {
                  user({
                      "while after if"
                  })
                  state[[2L]] <- 5L
                  state[[3L]] <- 1L
              }, `5` = {
                  repeat switch(state[[3L]], `1` = {
                      if (user({
                        TRUE
                      })) {
                        state[[3L]] <- 2L
                      } else {
                        break
                      }
                  }, `2` = {
                      user({
                        "while-while before if"
                      })
                      if (user({
                        j > 3
                      })) {
                        state[[3L]] <- 3L
                      } else {
                        state[[3L]] <- 4L
                      }
                      state[[4L]] <- 1L
                  }, `3` = {
                      repeat switch(state[[4L]], `1` = {
                        user({
                          "while-while-if before break"
                          "break"
                        })
                        length(state) <- 2L
                        break
                      }, `2` = {
                        break
                      })
                      n <- length(state)
                      if (n < 3L) {
                        break
                      }
                      if (n == 3L) {
                        state[[3L]] <- 1L
                        next
                      }
                      length(state) <- 3L
                      state[[3L]] <- 4L
                  }, `4` = {
                      user({
                        "while-while after if"
                      })
                      state[[3L]] <- 1L
                  })
                  length(state) <- 2L
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      generator_body(function() {
        while (1) {
          while (2) {
            yield(1)
            break
          }
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      1
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  state[[2L]] <- 3L
                  state[[3L]] <- 1L
              }, `3` = {
                  repeat switch(state[[3L]], `1` = {
                      if (user({
                        2
                      })) {
                        state[[3L]] <- 2L
                      } else {
                        break
                      }
                  }, `2` = {
                      validate_yield(user({
                        1
                      }))
                      state[[3L]] <- 3L
                      suspend()
                      return(last_value())
                  }, `3` = {
                      user({
                        "break"
                      })
                      break
                  })
                  length(state) <- 2L
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

