# `if` blocks - one pause

    Code
      generator_body(function() {
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
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
              })
              if (user({
                  TRUE
              })) {
                  state[[1L]] <- 2L
              } else {
                  state[[1L]] <- 3L
              }
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "if-before"
                      1L
                  })
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 3L
              }, `3` = {
                  user({
                      "if-after"
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
                      FALSE
                  })
                  state[[2L]] <- 2L
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
              state[[1L]] <- 4L
          }, `4` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# `else` blocks - one pause

    Code
      generator_body(function() {
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
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
              })
              if (user({
                  FALSE
              })) {
                  state[[1L]] <- 2L
              } else {
                  state[[1L]] <- 3L
              }
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      FALSE
                  })
                  state[[2L]] <- 2L
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
              state[[1L]] <- 4L
          }, `3` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "else-before"
                      1L
                  })
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 3L
              }, `3` = {
                  user({
                      "else-after"
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
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# `if` blocks - inner block

    Code
      generator_body(function() {
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
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
              })
              if (user({
                  TRUE
              })) {
                  state[[1L]] <- 2L
              } else {
                  state[[1L]] <- 3L
              }
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "if-before"
                      "inner-before"
                      1L
                  })
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 3L
              }, `3` = {
                  user({
                      "inner-after"
                      "if-after"
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
                      FALSE
                  })
                  state[[2L]] <- 2L
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
              state[[1L]] <- 4L
          }, `4` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# `if` blocks - nested

    Code
      generator_body(function() {
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
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
              })
              if (user({
                  TRUE
              })) {
                  state[[1L]] <- 2L
              } else {
                  state[[1L]] <- 3L
              }
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "if-before"
                  })
                  if (user({
                      FALSE
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      state[[2L]] <- 3L
                  }
                  state[[3L]] <- 1L
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user(1L)
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
                  user({
                      "if-after"
                  })
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
                      "foo"
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

# `if` blocks - nested and trailing pause

    Code
      generator_body(function() {
        "before"
        if (TRUE) {
          "if-before"
          if (FALSE) yield(1L)
        } else {
          "foo"
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
              })
              if (user({
                  TRUE
              })) {
                  state[[1L]] <- 2L
              } else {
                  state[[1L]] <- 3L
              }
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "if-before"
                  })
                  if (user({
                      FALSE
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      state[[2L]] <- 3L
                  }
                  state[[3L]] <- 1L
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user(1L)
                      state[[3L]] <- 2L
                      suspend()
                      return(last_value())
                  }, `2` = {
                      .last_value <- if (missing(arg)) exhausted() else arg
                      state[[3L]] <- 3L
                  }, `3` = {
                      exhausted <- TRUE
                      return(last_value())
                  }, `4` = {
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
              break
          }, `3` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "foo"
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

# `if` blocks - multiply nested and all trailing

    Code
      generator_body(function() {
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
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
              })
              if (user({
                  TRUE
              })) {
                  state[[1L]] <- 2L
              } else {
                  state[[1L]] <- 3L
              }
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "if-before"
                  })
                  if (user({
                      FALSE
                  })) {
                      state[[2L]] <- 2L
                  } else {
                      state[[2L]] <- 3L
                  }
                  state[[3L]] <- 1L
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      if (user({
                        FALSE
                      })) {
                        state[[3L]] <- 2L
                      } else {
                        state[[3L]] <- 3L
                      }
                      state[[4L]] <- 1L
                  }, `2` = {
                      repeat switch(state[[4L]], `1` = {
                        user({
                          1L
                        })
                        state[[4L]] <- 2L
                        suspend()
                        return(last_value())
                      }, `2` = {
                        .last_value <- if (missing(arg)) NULL else arg
                        state[[4L]] <- 3L
                      }, `3` = {
                        user({
                          "if-3-after"
                        })
                        state[[4L]] <- 4L
                      }, `4` = {
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
                      state[[3L]] <- 3L
                  }, `3` = {
                      user({
                        "if-2-after"
                      })
                      exhausted <- TRUE
                      return(last_value())
                  }, `4` = {
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
              break
          }, `3` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      FALSE
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

# `if`-`else` blocks - trailing

    Code
      generator_body(function() {
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
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
              })
              if (user({
                  TRUE
              })) {
                  state[[1L]] <- 2L
              } else {
                  state[[1L]] <- 3L
              }
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "if-before"
                      1L
                  })
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 3L
              }, `3` = {
                  user({
                      "if-after"
                  })
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
                      "else-before"
                      2L
                  })
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 3L
              }, `3` = {
                  user({
                      "else-after"
                  })
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
          }, `4` = {
              break
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# `if`-`else` blocks - non trailing

    Code
      generator_body(function() {
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
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
              })
              if (user({
                  TRUE
              })) {
                  state[[1L]] <- 2L
              } else {
                  state[[1L]] <- 3L
              }
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "if-before"
                      1L
                  })
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 3L
              }, `3` = {
                  user({
                      "if-after"
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
                      "else-before"
                      2L
                  })
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 3L
              }, `3` = {
                  user({
                      "else-after"
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
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# `if`-`else` blocks - same continuation

    Code
      generator_body(function() {
        "before"
        if (TRUE) {
          yield(1L)
        } else {
          yield(2L)
        }
        "after"
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
              })
              if (user({
                  TRUE
              })) {
                  state[[1L]] <- 2L
              } else {
                  state[[1L]] <- 3L
              }
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      1L
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
                      2L
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
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# `if`-`else` blocks - continuation in `if`

    Code
      generator_body(function() {
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
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
              })
              if (user({
                  TRUE
              })) {
                  state[[1L]] <- 2L
              } else {
                  state[[1L]] <- 3L
              }
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      1L
                  })
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 3L
              }, `3` = {
                  user({
                      "if-after"
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
                      2L
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
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# `if`-`else` blocks - continuation in `else`

    Code
      generator_body(function() {
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
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
              })
              if (user({
                  TRUE
              })) {
                  state[[1L]] <- 2L
              } else {
                  state[[1L]] <- 3L
              }
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      1L
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
                      2L
                  })
                  state[[2L]] <- 2L
                  suspend()
                  return(last_value())
              }, `2` = {
                  .last_value <- if (missing(arg)) NULL else arg
                  state[[2L]] <- 3L
              }, `3` = {
                  user({
                      "else-after"
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
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# `if` blocks - doubly nested with continuation

    Code
      generator_body(function() {
        if (TRUE) {
          if (TRUE) {
            yield(1L)
            "if-3-after"
          }
        }
        "after"
      })
    Output
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              if (user({
                  TRUE
              })) {
                  state[[1L]] <- 2L
              } else {
                  state[[1L]] <- 3L
              }
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
                        1L
                      })
                      state[[3L]] <- 2L
                      suspend()
                      return(last_value())
                  }, `2` = {
                      .last_value <- if (missing(arg)) NULL else arg
                      state[[3L]] <- 3L
                  }, `3` = {
                      user({
                        "if-3-after"
                      })
                      state[[3L]] <- 4L
                  }, `4` = {
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
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

# `if`-`else` blocks - multiply nested and not trailing

    Code
      generator_body(function() {
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
      {
          if (exhausted) {
              return(invisible(exhausted()))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "before"
              })
              if (user({
                  TRUE
              })) {
                  state[[1L]] <- 2L
              } else {
                  state[[1L]] <- 3L
              }
              state[[2L]] <- 1L
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "if-before"
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
                        user({
                          1L
                        })
                        state[[4L]] <- 2L
                        suspend()
                        return(last_value())
                      }, `2` = {
                        .last_value <- if (missing(arg)) NULL else arg
                        state[[4L]] <- 3L
                      }, `3` = {
                        user({
                          "if-3-after"
                        })
                        state[[4L]] <- 4L
                      }, `4` = {
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
                      state[[3L]] <- 3L
                  }, `3` = {
                      user({
                        "if-2-after"
                      })
                      state[[3L]] <- 4L
                  }, `4` = {
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
                      if (user({
                        FALSE
                      })) {
                        state[[3L]] <- 2L
                      } else {
                        state[[3L]] <- 3L
                      }
                      state[[4L]] <- 1L
                  }, `2` = {
                      repeat switch(state[[4L]], `1` = {
                        user({
                          FALSE
                        })
                        state[[4L]] <- 2L
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
                        user({
                          2L
                        })
                        state[[4L]] <- 2L
                        suspend()
                        return(last_value())
                      }, `2` = {
                        .last_value <- if (missing(arg)) NULL else arg
                        state[[4L]] <- 3L
                      }, `3` = {
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
                      FALSE
                  })
                  state[[2L]] <- 2L
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
              state[[1L]] <- 4L
          }, `4` = {
              user({
                  "after"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(exhausted())
      }

