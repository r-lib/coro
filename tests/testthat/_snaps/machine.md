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
              pop_machine()
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
                  pop_to_loop()
                  break
              })
              pop_machine()
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
              pop_machine()
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
              pop_machine()
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
                  pop_machine()
                  break
              })
              pop_machine()
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
                  pop_machine()
                  goto(3L)
              }, `3` = {
                  user({
                      "after"
                  })
                  goto(1L)
              })
              pop_machine()
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
                  pop_machine()
                  goto(3L)
              }, `3` = {
                  user({
                      "break"
                  })
                  pop_to_loop()
                  break
              })
              pop_machine()
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
                      pop_to_loop()
                      break
                  })
                  pop_machine()
                  goto(3L)
              }, `3` = {
                  user({
                      "break"
                  })
                  pop_to_loop()
                  break
              })
              pop_machine()
              break
          })
          kill()
          invisible(NULL)
      }

