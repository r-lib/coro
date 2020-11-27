# generators have return states

    Code
      generator_body(function() "foo")
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user("foo")
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() return("foo"))
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user("foo")
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# generators have yield states

    Code
      generator_body(function() yield("foo"))
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user("foo")
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) exhausted() else arg
              state[[1L]] <- 3L
          }, `3` = {
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() coro::yield("foo"))
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user("foo")
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) exhausted() else arg
              state[[1L]] <- 3L
          }, `3` = {
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# generators support blocks

    Code
      generator_body(function() {
        "foo"
        "bar"
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "foo"
                  "bar"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        "foo"
        yield("value")
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "foo"
                  "value"
              })
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) exhausted() else arg
              state[[1L]] <- 3L
          }, `3` = {
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        "foo"
        return("value")
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "foo"
                  "value"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        "foo"
        yield("value")
        "bar"
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "foo"
                  "value"
              })
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) NULL else arg
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "bar"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        "foo"
        yield("value")
        return("bar")
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "foo"
                  "value"
              })
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) NULL else arg
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "bar"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# generators support repeat loops

    Code
      generator_body(function() {
        repeat yield("value")
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user("value")
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        body1()
        repeat {
          yield("value")
          break
        }
        body2()
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "value"
                  })
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 3L
              }, `3` = {
                  user({
                      "break"
                  })
                  break
              })
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  body2()
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        body1()
        repeat {
          body2()
          yield("value")
          body3()
        }
        body4()
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      body2()
                      "value"
                  })
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 3L
              }, `3` = {
                  user({
                      body3()
                  })
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  body4()
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        repeat next
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        body1()
        repeat {
          body2()
          next
          body3()
        }
        body4()
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      body2()
                      "next"
                  })
                  state[[2L]] <- 1L
              }, `2` = {
                  user({
                      body3()
                  })
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  body4()
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# generators support while loops

    Code
      generator_body(function() {
        while (loop_condition) {
          body2()
          yield("value")
          body3()
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      loop_condition
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  user({
                      body2()
                      "value"
                  })
                  state[[2L]] <- 3L
                  suspend()
                  return(last_value())
              }, `3` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 4L
              }, `4` = {
                  user({
                      body3()
                  })
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# generators support nested loops

    Code
      generator_body(function() {
        repeat {
          repeat yield("foo")
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
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
                      "repeat"
                  })
                  state[[2L]] <- 2L
                  state[[3L]] <- 1L
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user("foo")
                      state[[3L]] <- 2L
                      suspend()
                      return(last_value())
                  }, `2` = {
                      .last_value <- if (missing(arg)) NULL else arg
                      state[[3L]] <- 1L
                  })
                  length(state) <- 2L
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        repeat repeat yield("foo")
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  state[[2L]] <- 2L
                  state[[3L]] <- 1L
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user("foo")
                      state[[3L]] <- 2L
                      suspend()
                      return(last_value())
                  }, `2` = {
                      .last_value <- if (missing(arg)) NULL else arg
                      state[[3L]] <- 1L
                  })
                  length(state) <- 2L
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        repeat while (TRUE) yield("foo")
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  state[[2L]] <- 2L
                  state[[3L]] <- 1L
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      if (user(TRUE)) {
                        state[[3L]] <- 2L
                      } else {
                        break
                      }
                  }, `2` = {
                      user("foo")
                      state[[3L]] <- 3L
                      suspend()
                      return(last_value())
                  }, `3` = {
                      .last_value <- if (missing(arg)) NULL else arg
                      state[[3L]] <- 1L
                  })
                  length(state) <- 2L
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        repeat {
          repeat yield("foo")
          "after"
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
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
                      "repeat"
                  })
                  state[[2L]] <- 2L
                  state[[3L]] <- 1L
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user("foo")
                      state[[3L]] <- 2L
                      suspend()
                      return(last_value())
                  }, `2` = {
                      .last_value <- if (missing(arg)) NULL else arg
                      state[[3L]] <- 1L
                  })
                  length(state) <- 2L
                  state[[2L]] <- 3L
              }, `3` = {
                  user({
                      "after"
                  })
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        repeat {
          repeat yield("foo")
          break
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
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
                      "repeat"
                  })
                  state[[2L]] <- 2L
                  state[[3L]] <- 1L
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user("foo")
                      state[[3L]] <- 2L
                      suspend()
                      return(last_value())
                  }, `2` = {
                      .last_value <- if (missing(arg)) NULL else arg
                      state[[3L]] <- 1L
                  })
                  length(state) <- 2L
                  state[[2L]] <- 3L
              }, `3` = {
                  user({
                      "break"
                  })
                  break
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        repeat {
          repeat break
          break
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
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
                      "repeat"
                  })
                  state[[2L]] <- 2L
                  state[[3L]] <- 1L
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      break
                  })
                  length(state) <- 2L
                  state[[2L]] <- 3L
              }, `3` = {
                  user({
                      "break"
                  })
                  break
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# generators support if-else branches

    Code
      generator_body(function() {
        body1()
        if (condition) {
          yield("then")
        }
        body2()
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
              })
              if (user({
                  condition
              })) {
                  state[[1L]] <- 2L
              } else {
                  state[[1L]] <- 3L
              }
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "then"
                  })
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 3L
              }, `3` = {
                  break
              })
              n <- length(state)
              if (n < 1L) {
                  break
              }
              if (n == 1L) {
                  state[[1L]] <- 1L
                  next
              }
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  body2()
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        body1()
        if (condition) {
          yield("then")
        } else {
          yield("else")
        }
        body2()
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
              })
              if (user({
                  condition
              })) {
                  state[[1L]] <- 2L
              } else {
                  state[[1L]] <- 3L
              }
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "then"
                  })
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 3L
              }, `3` = {
                  break
              })
              n <- length(state)
              if (n < 1L) {
                  break
              }
              if (n == 1L) {
                  state[[1L]] <- 1L
                  next
              }
              length(state) <- 1L
              state[[1L]] <- 4L
          }, `3` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "else"
                  })
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 3L
              }, `3` = {
                  break
              })
              n <- length(state)
              if (n < 1L) {
                  break
              }
              if (n == 1L) {
                  state[[1L]] <- 1L
                  next
              }
              length(state) <- 1L
              state[[1L]] <- 4L
          }, `4` = {
              user({
                  body2()
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        body1()
        if (condition) {
          then1()
          yield("then")
          then2()
        } else {
          else1()
          yield("else")
          else2()
        }
        body2()
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
              })
              if (user({
                  condition
              })) {
                  state[[1L]] <- 2L
              } else {
                  state[[1L]] <- 3L
              }
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      then1()
                      "then"
                  })
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 3L
              }, `3` = {
                  user({
                      then2()
                  })
                  state[[2L]] <- 4L
              }, `4` = {
                  break
              })
              n <- length(state)
              if (n < 1L) {
                  break
              }
              if (n == 1L) {
                  state[[1L]] <- 1L
                  next
              }
              length(state) <- 1L
              state[[1L]] <- 4L
          }, `3` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      else1()
                      "else"
                  })
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 3L
              }, `3` = {
                  user({
                      else2()
                  })
                  state[[2L]] <- 4L
              }, `4` = {
                  break
              })
              n <- length(state)
              if (n < 1L) {
                  break
              }
              if (n == 1L) {
                  state[[1L]] <- 1L
                  next
              }
              length(state) <- 1L
              state[[1L]] <- 4L
          }, `4` = {
              user({
                  body2()
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        if (condition) {
          yield("then")
        } else {
          "else"
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              if (user({
                  condition
              })) {
                  state[[1L]] <- 2L
              } else {
                  state[[1L]] <- 3L
              }
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "then"
                  })
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  .last_value <- if (missing(arg)) exhausted() else arg
                  state[[2L]] <- 3L
              }, `3` = {
                  exhausted <- TRUE
                  return(last_value())
              }, `4` = {
                  break
              })
              n <- length(state)
              if (n < 1L) {
                  break
              }
              if (n == 1L) {
                  state[[1L]] <- 1L
                  next
              }
              length(state) <- 1L
              break
          }, `3` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "else"
                  })
                  exhausted <- TRUE
                  return(last_value())
              }, `2` = {
                  break
              })
              n <- length(state)
              if (n < 1L) {
                  break
              }
              if (n == 1L) {
                  state[[1L]] <- 1L
                  next
              }
              length(state) <- 1L
              break
          }, `4` = {
              break
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# break within if

    Code
      generator_body(function() {
        body1()
        repeat {
          if (condition) {
            break
          }
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      condition
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      state[[2L]] <- 3L
                  }
                  state[[3L]] <- 1L
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user({
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
                  state[[2L]] <- 1L
              }, `3` = {
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        body1()
        repeat {
          if (condition) {
            break
          }
          body2()
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
                  "repeat"
              })
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      condition
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      state[[2L]] <- 3L
                  }
                  state[[3L]] <- 1L
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user({
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
                  state[[2L]] <- 3L
              }, `3` = {
                  user({
                      body2()
                  })
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        body1()
        if (truth1) if (truth2) yield("value")
        body2()
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
              })
              if (user({
                  truth1
              })) {
                  state[[1L]] <- 2L
              } else {
                  state[[1L]] <- 3L
              }
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user(truth2)) {
                      state[[2L]] <- 2L
                  } else {
                      state[[2L]] <- 3L
                  }
                  state[[3L]] <- 1L
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user("value")
                      state[[3L]] <- 2L
                      suspend()
                      return(last_value())
                  }, `2` = {
                      .last_value <- if (missing(arg)) NULL else arg
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
                  state[[2L]] <- 3L
              }, `3` = {
                  break
              })
              n <- length(state)
              if (n < 1L) {
                  break
              }
              if (n == 1L) {
                  state[[1L]] <- 1L
                  next
              }
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  body2()
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# generators support if within loops

    Code
      generator_body(function() {
        repeat {
          if (TRUE) {
            break
          }
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
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
                      state[[2L]] <- 3L
                  }
                  state[[3L]] <- 1L
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user({
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
                  state[[2L]] <- 1L
              }, `3` = {
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        repeat {
          body1()
          if (TRUE) {
            break
          } else {
            next
          }
          body2()
        }
        body3()
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
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
                      body1()
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
                      user({
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
              }, `3` = {
                  repeat switch(state[[3L]], `1` = {
                      user({
                        "next"
                      })
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
                      body2()
                  })
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  body3()
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# next and break within two layers of if-else

    Code
      generator_body(function() {
        repeat {
          body1()
          if (TRUE) {
            if (TRUE) next else break
            body2()
          }
          body3()
        }
        body4()
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
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
                      body1()
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
                      if (user({
                        TRUE
                      })) {
                        state[[3L]] <- 2L
                      } else {
                        state[[3L]] <- 3L
                      }
                      state[[4L]] <- 1L
                  }, `2` = {
                      repeat switch(state[[4L]], `1` = {
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
                  }, `3` = {
                      repeat switch(state[[4L]], `1` = {
                        length(state) <- 1L
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
                        body2()
                      })
                      state[[3L]] <- 5L
                  }, `5` = {
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
                      body3()
                  })
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  body4()
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# handle for loops

    Code
      generator_body(function() for (x in 1:3) yield(x))
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              iterators[[2L]] <- as_iterator(user(1:3))
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if ({
                      iterator <- iterators[[2L]]
                      if (is_exhausted(elt <- iterator())) {
                        FALSE
                      } else {
                        user_env[["x"]] <- elt
                        TRUE
                      }
                  }) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  user(x)
                  state[[2L]] <- 3L
                  suspend()
                  return(last_value())
              }, `3` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 1L
              })
              iterators[[2L]] <- NULL
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() for (x in 1:3) for (y in 2:4) yield(list(x, y)))
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              iterators[[2L]] <- as_iterator(user(1:3))
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if ({
                      iterator <- iterators[[2L]]
                      if (is_exhausted(elt <- iterator())) {
                        FALSE
                      } else {
                        user_env[["x"]] <- elt
                        TRUE
                      }
                  }) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  iterators[[3L]] <- as_iterator(user(2:4))
                  state[[2L]] <- 3L
                  state[[3L]] <- 1L
              }, `3` = {
                  repeat switch(state[[3L]], `1` = {
                      if ({
                        iterator <- iterators[[3L]]
                        if (is_exhausted(elt <- iterator())) {
                          FALSE
                        } else {
                          user_env[["y"]] <- elt
                          TRUE
                        }
                      }) {
                        state[[3L]] <- 2L
                      } else {
                        break
                      }
                  }, `2` = {
                      user(list(x, y))
                      state[[3L]] <- 3L
                      suspend()
                      return(last_value())
                  }, `3` = {
                      .last_value <- if (missing(arg)) NULL else arg
                      state[[3L]] <- 1L
                  })
                  iterators[[3L]] <- NULL
                  length(state) <- 2L
                  state[[2L]] <- 1L
              })
              iterators[[2L]] <- NULL
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        body1()
        for (x in 1:3) yield(x)
        body2()
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
              })
              iterators[[2L]] <- as_iterator(user(1:3))
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if ({
                      iterator <- iterators[[2L]]
                      if (is_exhausted(elt <- iterator())) {
                        FALSE
                      } else {
                        user_env[["x"]] <- elt
                        TRUE
                      }
                  }) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  user(x)
                  state[[2L]] <- 3L
                  suspend()
                  return(last_value())
              }, `3` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 1L
              })
              iterators[[2L]] <- NULL
              length(state) <- 1L
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  body2()
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# handle yield-assign

    Code
      generator_body(function(x) x <- yield(x))
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user(x)
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- user_env[["x"]] <- if (missing(arg)) NULL else arg
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function(x) {
        x <- yield(x)
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  x
              })
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- user_env[["x"]] <- if (missing(arg)) NULL else arg
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# tryCatch() expressions are treated as normal expressions if possible

    Code
      generator_body(function() tryCatch(foo()))
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user(tryCatch(foo()))
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# tryCatch() expressions are parsed

    Code
      generator_body(function() {
        tryCatch(error = function(...) "handled", {
          stop("error")
          yield("yield")
        })
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "tryCatch"
              })
              handlers[[2L]] <- user(base::list(error = function(...) "handled"))
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              .last_value <- with_try_catch(handlers[[2L]], {
                  {
                      repeat switch(state[[2L]], `1` = {
                        user({
                          stop("error")
                          "yield"
                        })
                        state[[2L]] <- 2L
                        suspend()
                        return(last_value())
                      }, `2` = {
                        .last_value <- if (missing(arg)) exhausted() else arg
                        state[[2L]] <- 3L
                      }, `3` = {
                        exhausted <- TRUE
                        return(last_value())
                      }, `4` = {
                        break
                      })
                      n <- length(state)
                      if (n < 1L) {
                        break
                      }
                      if (n == 1L) {
                        state[[1L]] <- 1L
                        next
                      }
                      length(state) <- 1L
                      break
                  }
                  last_value()
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      generator_body(function() {
        tryCatch(error = function(...) "handled", {
          stop("error")
          yield("yield")
        })
        "value"
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "tryCatch"
              })
              handlers[[2L]] <- user(base::list(error = function(...) "handled"))
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              with_try_catch(handlers[[2L]], {
                  {
                      repeat switch(state[[2L]], `1` = {
                        user({
                          stop("error")
                          "yield"
                        })
                        state[[2L]] <- 2L
                        suspend()
                        return(last_value())
                      }, `2` = {
                        .last_value <- if (missing(arg)) NULL else arg
                        state[[2L]] <- 3L
                      }, `3` = {
                        break
                      })
                      n <- length(state)
                      if (n < 1L) {
                        break
                      }
                      if (n == 1L) {
                        state[[1L]] <- 1L
                        next
                      }
                      length(state) <- 1L
                      state[[1L]] <- 3L
                  }
                  last_value()
              })
              state[[1L]] <- 3L
          }, `3` = {
              user({
                  "value"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# withCallingHandlers() expressions are parsed

    Code
      generator_body(function() withCallingHandlers(expr))
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user(withCallingHandlers(expr))
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# tryCatch() can be assigned

    Code
      generator_body(function() {
        value <- tryCatch(error = function(...) "handled", {
          stop("error")
          yield("yield")
        })
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "tryCatch"
              })
              handlers[[2L]] <- user(base::list(error = function(...) "handled"))
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              .last_value <- user_env[["value"]] <- with_try_catch(handlers[[2L]], 
                  {
                      {
                        repeat switch(state[[2L]], `1` = {
                          user({
                            stop("error")
                            "yield"
                          })
                          state[[2L]] <- 2L
                          suspend()
                          return(last_value())
                        }, `2` = {
                          .last_value <- if (missing(arg)) NULL else arg
                          state[[2L]] <- 3L
                        }, `3` = {
                          break
                        })
                        n <- length(state)
                        if (n < 1L) {
                          break
                        }
                        if (n == 1L) {
                          state[[1L]] <- 1L
                          next
                        }
                        length(state) <- 1L
                        state[[1L]] <- 3L
                      }
                      last_value()
                  })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# trailing `await()` returns the awaited value wrapped in a promise

    Code
      async_body(function() await(x))
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              .last_value <- then(as_promise(user(x)), callback = .self)
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- if (missing(arg)) exhausted() else arg
              state[[1L]] <- 3L
          }, `3` = {
              exhausted <- TRUE
              return(as_promise(last_value()))
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      async_body(function() repeat await(x))
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  .last_value <- then(as_promise(user(x)), callback = .self)
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 1L
              })
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      async_body(function() while (x) if (y) await(z))
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user(x)) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  if (user(y)) {
                      state[[2L]] <- 3L
                  } else {
                      state[[2L]] <- 4L
                  }
                  state[[3L]] <- 1L
              }, `3` = {
                  repeat switch(state[[3L]], `1` = {
                      .last_value <- then(as_promise(user(z)), callback = .self)
                      state[[3L]] <- 2L
                      suspend()
                      return(last_value())
                  }, `2` = {
                      .last_value <- if (missing(arg)) NULL else arg
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
          invisible(exhausted())
      }

---

    Code
      async_body(function() for (x in y) await(z))
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              iterators[[2L]] <- as_iterator(user(y))
              state[[1L]] <- 2L
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if ({
                      iterator <- iterators[[2L]]
                      if (is_exhausted(elt <- iterator())) {
                        FALSE
                      } else {
                        user_env[["x"]] <- elt
                        TRUE
                      }
                  }) {
                      state[[2L]] <- 2L
                  } else {
                      break
                  }
              }, `2` = {
                  .last_value <- then(as_promise(user(z)), callback = .self)
                  state[[2L]] <- 3L
                  suspend()
                  return(last_value())
              }, `3` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 1L
              })
              iterators[[2L]] <- NULL
              length(state) <- 1L
              break
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

---

    Code
      async_body(function() x <- await(async_foo()))
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              .last_value <- then(as_promise(user(async_foo())), callback = .self)
              state[[1L]] <- 2L
              suspend()
              return(last_value())
          }, `2` = {
              .last_value <- user_env[["x"]] <- if (missing(arg)) NULL else arg
              exhausted <- TRUE
              return(as_promise(last_value()))
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

