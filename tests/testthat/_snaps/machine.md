# generators have return states

    Code
      generator_body(function() "foo")
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user("foo")
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      generator_body(function() return("foo"))
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user("foo")
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# generators have yield states

    Code
      generator_body(function() yield("foo"))
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user("foo")
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      generator_body(function() flowery::yield("foo"))
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user("foo")
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
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
              return(invisible(NULL))
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
          invisible(NULL)
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
              return(invisible(NULL))
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
          invisible(NULL)
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
              return(invisible(NULL))
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
          invisible(NULL)
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
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "foo"
                  "value"
              })
              suspend_to(2L)
              return(last_value())
          }, `2` = {
              user({
                  "bar"
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
        "foo"
        yield("value")
        return("bar")
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "foo"
                  "value"
              })
              suspend_to(2L)
              return(last_value())
          }, `2` = {
              user({
                  "bar"
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# generators support repeat loops

    Code
      generator_body(function() {
        repeat yield("value")
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
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user("value")
                  suspend_to(1L)
                  return(last_value())
              })
              set_depth(1L)
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      generator_body(function() {
        body1()
        repeat {
          yield("value")
          `break`()
        }
        body2()
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
                  "repeat"
              })
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "value"
                  })
                  suspend_to(2L)
                  return(last_value())
              }, `2` = {
                  user({
                      "break"
                  })
                  break
              })
              set_depth(1L)
              set_state(3L)
          }, `3` = {
              user({
                  body2()
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
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
                  "repeat"
              })
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      body2()
                      "value"
                  })
                  suspend_to(2L)
                  return(last_value())
              }, `2` = {
                  user({
                      body3()
                  })
                  set_state(1L)
              })
              set_depth(1L)
              set_state(3L)
          }, `3` = {
              user({
                  body4()
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
        repeat `next`()
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
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  set_state(1L)
              })
              set_depth(1L)
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      generator_body(function() {
        body1()
        repeat {
          body2()
          `next`()
          body3()
        }
        body4()
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
                  "repeat"
              })
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      body2()
                      "next"
                  })
                  set_state(1L)
              }, `2` = {
                  user({
                      body3()
                  })
                  set_state(1L)
              })
              set_depth(1L)
              set_state(3L)
          }, `3` = {
              user({
                  body4()
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
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
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      loop_condition
                  })) {
                      set_state(2L)
                  } else {
                      break
                  }
              }, `2` = {
                  user({
                      body2()
                      "value"
                  })
                  suspend_to(3L)
                  return(last_value())
              }, `3` = {
                  user({
                      body3()
                  })
                  set_state(1L)
              })
              set_depth(1L)
              break
          })
          exhausted <- TRUE
          invisible(NULL)
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
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "repeat"
                  })
                  set_state(2L)
                  set_depth(3L)
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user("foo")
                      suspend_to(1L)
                      return(last_value())
                  })
                  set_depth(2L)
                  break
              })
              set_depth(1L)
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      generator_body(function() {
        repeat repeat yield("foo")
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
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  set_state(2L)
                  set_depth(3L)
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user("foo")
                      suspend_to(1L)
                      return(last_value())
                  })
                  set_depth(2L)
                  break
              })
              set_depth(1L)
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      generator_body(function() {
        repeat while (TRUE) yield("foo")
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
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  set_state(2L)
                  set_depth(3L)
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      if (user(TRUE)) {
                        set_state(2L)
                      } else {
                        break
                      }
                  }, `2` = {
                      user("foo")
                      suspend_to(1L)
                      return(last_value())
                  })
                  set_depth(2L)
                  break
              })
              set_depth(1L)
              break
          })
          exhausted <- TRUE
          invisible(NULL)
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
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "repeat"
                  })
                  set_state(2L)
                  set_depth(3L)
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user("foo")
                      suspend_to(1L)
                      return(last_value())
                  })
                  set_depth(2L)
                  set_state(3L)
              }, `3` = {
                  user({
                      "after"
                  })
                  set_state(1L)
              })
              set_depth(1L)
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      generator_body(function() {
        repeat {
          repeat yield("foo")
          `break`()
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
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "repeat"
                  })
                  set_state(2L)
                  set_depth(3L)
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user("foo")
                      suspend_to(1L)
                      return(last_value())
                  })
                  set_depth(2L)
                  set_state(3L)
              }, `3` = {
                  user({
                      "break"
                  })
                  break
              })
              set_depth(1L)
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      generator_body(function() {
        repeat {
          repeat `break`()
          `break`()
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
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "repeat"
                  })
                  set_state(2L)
                  set_depth(3L)
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      break
                  })
                  set_depth(2L)
                  set_state(3L)
              }, `3` = {
                  user({
                      "break"
                  })
                  break
              })
              set_depth(1L)
              break
          })
          exhausted <- TRUE
          invisible(NULL)
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
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
              })
              if (user({
                  condition
              })) {
                  set_state(2L)
              } else {
                  set_state(3L)
              }
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "then"
                  })
                  suspend_to(2L)
                  return(last_value())
              }, `2` = {
                  break
              })
              n <- depth()
              if (n < 1L) break
              if (n == 1L) {
                  set_state(1L)
                  next
              }
              set_depth(1L)
              set_state(3L)
          }, `3` = {
              user({
                  body2()
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
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
              })
              if (user({
                  condition
              })) {
                  set_state(2L)
              } else {
                  set_state(3L)
              }
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "then"
                  })
                  suspend_to(2L)
                  return(last_value())
              }, `2` = {
                  break
              })
              n <- depth()
              if (n < 1L) break
              if (n == 1L) {
                  set_state(1L)
                  next
              }
              set_depth(1L)
              set_state(4L)
          }, `3` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "else"
                  })
                  suspend_to(2L)
                  return(last_value())
              }, `2` = {
                  break
              })
              n <- depth()
              if (n < 1L) break
              if (n == 1L) {
                  set_state(1L)
                  next
              }
              set_depth(1L)
              set_state(4L)
          }, `4` = {
              user({
                  body2()
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
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
              })
              if (user({
                  condition
              })) {
                  set_state(2L)
              } else {
                  set_state(3L)
              }
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      then1()
                      "then"
                  })
                  suspend_to(2L)
                  return(last_value())
              }, `2` = {
                  user({
                      then2()
                  })
                  set_state(3L)
              }, `3` = {
                  break
              })
              n <- depth()
              if (n < 1L) break
              if (n == 1L) {
                  set_state(1L)
                  next
              }
              set_depth(1L)
              set_state(4L)
          }, `3` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      else1()
                      "else"
                  })
                  suspend_to(2L)
                  return(last_value())
              }, `2` = {
                  user({
                      else2()
                  })
                  set_state(3L)
              }, `3` = {
                  break
              })
              n <- depth()
              if (n < 1L) break
              if (n == 1L) {
                  set_state(1L)
                  next
              }
              set_depth(1L)
              set_state(4L)
          }, `4` = {
              user({
                  body2()
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
        if (condition) {
          yield("then")
        } else {
          "else"
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              if (user({
                  condition
              })) {
                  set_state(2L)
              } else {
                  set_state(3L)
              }
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "then"
                  })
                  exhausted <- TRUE
                  return(last_value())
              }, `2` = {
                  break
              })
              n <- depth()
              if (n < 1L) break
              if (n == 1L) {
                  set_state(1L)
                  next
              }
              set_depth(1L)
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
              n <- depth()
              if (n < 1L) break
              if (n == 1L) {
                  set_state(1L)
                  next
              }
              set_depth(1L)
              break
          }, `4` = {
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# break within if

    Code
      generator_body(function() {
        body1()
        repeat {
          if (condition) {
            `break`()
          }
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
                  "repeat"
              })
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      condition
                  })) {
                      set_state(2L)
                  } else {
                      set_state(3L)
                  }
                  set_depth(3L)
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user({
                        "break"
                      })
                      set_depth(1L)
                      break
                  }, `2` = {
                      break
                  })
                  n <- depth()
                  if (n < 2L) break
                  if (n == 2L) {
                      set_state(1L)
                      next
                  }
                  set_depth(2L)
                  set_state(1L)
              }, `3` = {
                  break
              })
              set_depth(1L)
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      generator_body(function() {
        body1()
        repeat {
          if (condition) {
            `break`()
          }
          body2()
        }
      })
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
                  "repeat"
              })
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      condition
                  })) {
                      set_state(2L)
                  } else {
                      set_state(3L)
                  }
                  set_depth(3L)
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user({
                        "break"
                      })
                      set_depth(1L)
                      break
                  }, `2` = {
                      break
                  })
                  n <- depth()
                  if (n < 2L) break
                  if (n == 2L) {
                      set_state(1L)
                      next
                  }
                  set_depth(2L)
                  set_state(3L)
              }, `3` = {
                  user({
                      body2()
                  })
                  set_state(1L)
              })
              set_depth(1L)
              break
          })
          exhausted <- TRUE
          invisible(NULL)
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
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
              })
              if (user({
                  truth1
              })) {
                  set_state(2L)
              } else {
                  set_state(3L)
              }
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user(truth2)) {
                      set_state(2L)
                  } else {
                      set_state(3L)
                  }
                  set_depth(3L)
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user("value")
                      suspend_to(2L)
                      return(last_value())
                  }, `2` = {
                      break
                  })
                  n <- depth()
                  if (n < 2L) break
                  if (n == 2L) {
                      set_state(1L)
                      next
                  }
                  set_depth(2L)
                  set_state(3L)
              }, `3` = {
                  break
              })
              n <- depth()
              if (n < 1L) break
              if (n == 1L) {
                  set_state(1L)
                  next
              }
              set_depth(1L)
              set_state(3L)
          }, `3` = {
              user({
                  body2()
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# generators support if within loops

    Code
      generator_body(function() {
        repeat {
          if (TRUE) {
            `break`()
          }
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
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      TRUE
                  })) {
                      set_state(2L)
                  } else {
                      set_state(3L)
                  }
                  set_depth(3L)
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user({
                        "break"
                      })
                      set_depth(1L)
                      break
                  }, `2` = {
                      break
                  })
                  n <- depth()
                  if (n < 2L) break
                  if (n == 2L) {
                      set_state(1L)
                      next
                  }
                  set_depth(2L)
                  set_state(1L)
              }, `3` = {
                  break
              })
              set_depth(1L)
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      generator_body(function() {
        repeat {
          body1()
          if (TRUE) {
            `break`()
          } else {
            `next`()
          }
          body2()
        }
        body3()
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
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      body1()
                  })
                  if (user({
                      TRUE
                  })) {
                      set_state(2L)
                  } else {
                      set_state(3L)
                  }
                  set_depth(3L)
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user({
                        "break"
                      })
                      set_depth(1L)
                      break
                  }, `2` = {
                      break
                  })
                  n <- depth()
                  if (n < 2L) break
                  if (n == 2L) {
                      set_state(1L)
                      next
                  }
                  set_depth(2L)
                  set_state(4L)
              }, `3` = {
                  repeat switch(state[[3L]], `1` = {
                      user({
                        "next"
                      })
                      set_depth(2L)
                      break
                  }, `2` = {
                      break
                  })
                  n <- depth()
                  if (n < 2L) break
                  if (n == 2L) {
                      set_state(1L)
                      next
                  }
                  set_depth(2L)
                  set_state(4L)
              }, `4` = {
                  user({
                      body2()
                  })
                  set_state(1L)
              })
              set_depth(1L)
              set_state(3L)
          }, `3` = {
              user({
                  body3()
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# next and break within two layers of if-else

    Code
      generator_body(function() {
        repeat {
          body1()
          if (TRUE) {
            if (TRUE) `next`() else `break`()
            body2()
          }
          body3()
        }
        body4()
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
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      body1()
                  })
                  if (user({
                      TRUE
                  })) {
                      set_state(2L)
                  } else {
                      set_state(3L)
                  }
                  set_depth(3L)
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      if (user({
                        TRUE
                      })) {
                        set_state(2L)
                      } else {
                        set_state(3L)
                      }
                      set_depth(4L)
                  }, `2` = {
                      repeat switch(state[[4L]], `1` = {
                        set_depth(2L)
                        break
                      }, `2` = {
                        break
                      })
                      n <- depth()
                      if (n < 3L) break
                      if (n == 3L) {
                        set_state(1L)
                        next
                      }
                      set_depth(3L)
                      set_state(4L)
                  }, `3` = {
                      repeat switch(state[[4L]], `1` = {
                        set_depth(1L)
                        break
                      }, `2` = {
                        break
                      })
                      n <- depth()
                      if (n < 3L) break
                      if (n == 3L) {
                        set_state(1L)
                        next
                      }
                      set_depth(3L)
                      set_state(4L)
                  }, `4` = {
                      user({
                        body2()
                      })
                      set_state(5L)
                  }, `5` = {
                      break
                  })
                  n <- depth()
                  if (n < 2L) break
                  if (n == 2L) {
                      set_state(1L)
                      next
                  }
                  set_depth(2L)
                  set_state(3L)
              }, `3` = {
                  user({
                      body3()
                  })
                  set_state(1L)
              })
              set_depth(1L)
              set_state(3L)
          }, `3` = {
              user({
                  body4()
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

# handle for loops

    Code
      generator_body(function() for (x in 1:3) yield(x))
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              iterators[[2L]] <- as_iterator(user(1:3))
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if ({
                      iterator <- iterators[[2L]]
                      if (is_null(elt <- iterator())) {
                        FALSE
                      } else {
                        user_env[["x"]] <- elt
                        TRUE
                      }
                  }) {
                      set_state(2L)
                  } else {
                      break
                  }
              }, `2` = {
                  user(x)
                  suspend_to(1L)
                  return(last_value())
              })
              set_depth(1L)
              iterators[[2L]] <- NULL
              break
          })
          exhausted <- TRUE
          invisible(NULL)
      }

---

    Code
      generator_body(function() for (x in 1:3) for (y in 2:4) yield(list(x, y)))
    Output
      {
          if (exhausted) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              iterators[[2L]] <- as_iterator(user(1:3))
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if ({
                      iterator <- iterators[[2L]]
                      if (is_null(elt <- iterator())) {
                        FALSE
                      } else {
                        user_env[["x"]] <- elt
                        TRUE
                      }
                  }) {
                      set_state(2L)
                  } else {
                      break
                  }
              }, `2` = {
                  iterators[[3L]] <- as_iterator(user(2:4))
                  set_state(3L)
                  set_depth(3L)
              }, `3` = {
                  repeat switch(state[[3L]], `1` = {
                      if ({
                        iterator <- iterators[[3L]]
                        if (is_null(elt <- iterator())) {
                          FALSE
                        } else {
                          user_env[["y"]] <- elt
                          TRUE
                        }
                      }) {
                        set_state(2L)
                      } else {
                        break
                      }
                  }, `2` = {
                      user(list(x, y))
                      suspend_to(1L)
                      return(last_value())
                  })
                  set_depth(2L)
                  iterators[[3L]] <- NULL
                  break
              })
              set_depth(1L)
              iterators[[2L]] <- NULL
              break
          })
          exhausted <- TRUE
          invisible(NULL)
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
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
              })
              iterators[[2L]] <- as_iterator(user(1:3))
              set_state(2L)
              set_depth(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if ({
                      iterator <- iterators[[2L]]
                      if (is_null(elt <- iterator())) {
                        FALSE
                      } else {
                        user_env[["x"]] <- elt
                        TRUE
                      }
                  }) {
                      set_state(2L)
                  } else {
                      break
                  }
              }, `2` = {
                  user(x)
                  suspend_to(1L)
                  return(last_value())
              })
              set_depth(1L)
              iterators[[2L]] <- NULL
              set_state(3L)
          }, `3` = {
              user({
                  body2()
              })
              exhausted <- TRUE
              return(last_value())
          })
          exhausted <- TRUE
          invisible(NULL)
      }

