# generators have return states

    Code
      generator_body(function() "foo")
    Output
      {
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user("foo")
              kill()
              return(last_value())
          })
          kill()
          invisible(NULL)
      }

---

    Code
      generator_body(function() return("foo"))
    Output
      {
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user("foo")
              kill()
              return(last_value())
          })
          kill()
          invisible(NULL)
      }

# generators have yield states

    Code
      generator_body(function() yield("foo"))
    Output
      {
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user("foo")
              kill()
              return(last_value())
          })
          kill()
          invisible(NULL)
      }

---

    Code
      generator_body(function() flowery::yield("foo"))
    Output
      {
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user("foo")
              kill()
              return(last_value())
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "foo"
                  "bar"
              })
              kill()
              return(last_value())
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "foo"
                  "value"
              })
              kill()
              return(last_value())
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "foo"
                  "value"
              })
              kill()
              return(last_value())
          })
          kill()
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
          if (killed()) {
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
              kill()
              return(last_value())
          })
          kill()
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
          if (killed()) {
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
              kill()
              return(last_value())
          })
          kill()
          invisible(NULL)
      }

# generators support repeat loops

    Code
      generator_body(function() {
        repeat yield("value")
      })
    Output
      {
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              push_machine("loop")
              goto(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user("value")
                  suspend_to(1L)
                  return(last_value())
              })
              set_depth(1L)
              break
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
                  "repeat"
              })
              push_machine("loop")
              goto(2L)
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
              goto(3L)
          }, `3` = {
              user({
                  body2()
              })
              kill()
              return(last_value())
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
                  "repeat"
              })
              push_machine("loop")
              goto(2L)
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
                  goto(1L)
              })
              set_depth(1L)
              goto(3L)
          }, `3` = {
              user({
                  body4()
              })
              kill()
              return(last_value())
          })
          kill()
          invisible(NULL)
      }

---

    Code
      generator_body(function() {
        repeat `next`()
      })
    Output
      {
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              push_machine("loop")
              goto(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  goto(1L)
              })
              set_depth(1L)
              break
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
                  "repeat"
              })
              push_machine("loop")
              goto(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      body2()
                      "next"
                  })
                  goto(1L)
              }, `2` = {
                  user({
                      body3()
                  })
                  goto(1L)
              })
              set_depth(1L)
              goto(3L)
          }, `3` = {
              user({
                  body4()
              })
              kill()
              return(last_value())
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              push_machine("loop")
              goto(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  if (user({
                      loop_condition
                  })) {
                      goto(2L)
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
                  goto(1L)
              })
              set_depth(1L)
              break
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              push_machine("loop")
              goto(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "repeat"
                  })
                  push_machine("loop")
                  goto(2L)
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
          kill()
          invisible(NULL)
      }

---

    Code
      generator_body(function() {
        repeat repeat yield("foo")
      })
    Output
      {
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              push_machine("loop")
              goto(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  push_machine("loop")
                  goto(2L)
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
          kill()
          invisible(NULL)
      }

---

    Code
      generator_body(function() {
        repeat while (TRUE) yield("foo")
      })
    Output
      {
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              push_machine("loop")
              goto(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  push_machine("loop")
                  goto(2L)
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      if (user(TRUE)) {
                        goto(2L)
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
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              push_machine("loop")
              goto(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "repeat"
                  })
                  push_machine("loop")
                  goto(2L)
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user("foo")
                      suspend_to(1L)
                      return(last_value())
                  })
                  set_depth(2L)
                  goto(3L)
              }, `3` = {
                  user({
                      "after"
                  })
                  goto(1L)
              })
              set_depth(1L)
              break
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              push_machine("loop")
              goto(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "repeat"
                  })
                  push_machine("loop")
                  goto(2L)
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user("foo")
                      suspend_to(1L)
                      return(last_value())
                  })
                  set_depth(2L)
                  goto(3L)
              }, `3` = {
                  user({
                      "break"
                  })
                  break
              })
              set_depth(1L)
              break
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              push_machine("loop")
              goto(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "repeat"
                  })
                  push_machine("loop")
                  goto(2L)
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      break
                  })
                  set_depth(2L)
                  goto(3L)
              }, `3` = {
                  user({
                      "break"
                  })
                  break
              })
              set_depth(1L)
              break
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
              })
              push_machine("if")
              if (user({
                  condition
              })) {
                  goto(2L)
              } else {
                  goto(3L)
              }
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
              if (n == 1L) goto(1L)
              set_depth(1L)
              goto(3L)
          }, `3` = {
              user({
                  body2()
              })
              kill()
              return(last_value())
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
              })
              push_machine("if")
              if (user({
                  condition
              })) {
                  goto(2L)
              } else {
                  goto(3L)
              }
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
              if (n == 1L) goto(1L)
              set_depth(1L)
              goto(4L)
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
              if (n == 1L) goto(1L)
              set_depth(1L)
              goto(4L)
          }, `4` = {
              user({
                  body2()
              })
              kill()
              return(last_value())
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
              })
              push_machine("if")
              if (user({
                  condition
              })) {
                  goto(2L)
              } else {
                  goto(3L)
              }
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
                  goto(3L)
              }, `3` = {
                  break
              })
              n <- depth()
              if (n < 1L) break
              if (n == 1L) goto(1L)
              set_depth(1L)
              goto(4L)
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
                  goto(3L)
              }, `3` = {
                  break
              })
              n <- depth()
              if (n < 1L) break
              if (n == 1L) goto(1L)
              set_depth(1L)
              goto(4L)
          }, `4` = {
              user({
                  body2()
              })
              kill()
              return(last_value())
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              push_machine("if")
              if (user({
                  condition
              })) {
                  goto(2L)
              } else {
                  goto(3L)
              }
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "then"
                  })
                  kill()
                  return(last_value())
              }, `2` = {
                  break
              })
              n <- depth()
              if (n < 1L) break
              if (n == 1L) goto(1L)
              set_depth(1L)
              break
          }, `3` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      "else"
                  })
                  kill()
                  return(last_value())
              }, `2` = {
                  break
              })
              n <- depth()
              if (n < 1L) break
              if (n == 1L) goto(1L)
              set_depth(1L)
              break
          }, `4` = {
              break
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
                  "repeat"
              })
              push_machine("loop")
              goto(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  push_machine("if")
                  if (user({
                      condition
                  })) {
                      goto(2L)
                  } else {
                      goto(3L)
                  }
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
                  if (n == 2L) goto(1L)
                  set_depth(2L)
                  goto(1L)
              }, `3` = {
                  break
              })
              set_depth(1L)
              break
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
                  "repeat"
              })
              push_machine("loop")
              goto(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  push_machine("if")
                  if (user({
                      condition
                  })) {
                      goto(2L)
                  } else {
                      goto(3L)
                  }
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
                  if (n == 2L) goto(1L)
                  set_depth(2L)
                  goto(3L)
              }, `3` = {
                  user({
                      body2()
                  })
                  goto(1L)
              })
              set_depth(1L)
              break
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  body1()
              })
              push_machine("if")
              if (user({
                  truth1
              })) {
                  goto(2L)
              } else {
                  goto(3L)
              }
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  push_machine("if")
                  if (user(truth2)) {
                      goto(2L)
                  } else {
                      goto(3L)
                  }
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      user("value")
                      suspend_to(2L)
                      return(last_value())
                  }, `1` = {
                      break
                  })
                  n <- depth()
                  if (n < 2L) break
                  if (n == 2L) goto(1L)
                  set_depth(2L)
                  goto(3L)
              }, `2` = {
                  break
              })
              n <- depth()
              if (n < 1L) break
              if (n == 1L) goto(1L)
              set_depth(1L)
              goto(3L)
          }, `3` = {
              user({
                  body2()
              })
              kill()
              return(last_value())
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              push_machine("loop")
              goto(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  push_machine("if")
                  if (user({
                      TRUE
                  })) {
                      goto(2L)
                  } else {
                      goto(3L)
                  }
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
                  if (n == 2L) goto(1L)
                  set_depth(2L)
                  goto(1L)
              }, `3` = {
                  break
              })
              set_depth(1L)
              break
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              push_machine("loop")
              goto(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      body1()
                  })
                  push_machine("if")
                  if (user({
                      TRUE
                  })) {
                      goto(2L)
                  } else {
                      goto(3L)
                  }
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
                  if (n == 2L) goto(1L)
                  set_depth(2L)
                  goto(4L)
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
                  if (n == 2L) goto(1L)
                  set_depth(2L)
                  goto(4L)
              }, `4` = {
                  user({
                      body2()
                  })
                  goto(1L)
              })
              set_depth(1L)
              goto(3L)
          }, `3` = {
              user({
                  body3()
              })
              kill()
              return(last_value())
          })
          kill()
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
          if (killed()) {
              return(invisible(NULL))
          }
          repeat switch(state[[1L]], `1` = {
              user({
                  "repeat"
              })
              push_machine("loop")
              goto(2L)
          }, `2` = {
              repeat switch(state[[2L]], `1` = {
                  user({
                      body1()
                  })
                  push_machine("if")
                  if (user({
                      TRUE
                  })) {
                      goto(2L)
                  } else {
                      goto(3L)
                  }
              }, `2` = {
                  repeat switch(state[[3L]], `1` = {
                      push_machine("if")
                      if (user({
                        TRUE
                      })) {
                        goto(2L)
                      } else {
                        goto(3L)
                      }
                  }, `2` = {
                      repeat switch(state[[4L]], `1` = {
                        set_depth(2L)
                        break
                      }, `1` = {
                        break
                      })
                      n <- depth()
                      if (n < 3L) break
                      if (n == 3L) goto(1L)
                      set_depth(3L)
                      goto(4L)
                  }, `3` = {
                      repeat switch(state[[4L]], `1` = {
                        set_depth(1L)
                        break
                      }, `1` = {
                        break
                      })
                      n <- depth()
                      if (n < 3L) break
                      if (n == 3L) goto(1L)
                      set_depth(3L)
                      goto(4L)
                  }, `4` = {
                      user({
                        body2()
                      })
                      goto(5L)
                  }, `5` = {
                      break
                  })
                  n <- depth()
                  if (n < 2L) break
                  if (n == 2L) goto(1L)
                  set_depth(2L)
                  goto(3L)
              }, `3` = {
                  user({
                      body3()
                  })
                  goto(1L)
              })
              set_depth(1L)
              goto(3L)
          }, `3` = {
              user({
                  body4()
              })
              kill()
              return(last_value())
          })
          kill()
          invisible(NULL)
      }

