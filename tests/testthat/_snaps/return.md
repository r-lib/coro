# explicit return is added to blocks

    Code
      set_returns(function() {
        "foo"
      })
    Output
      {
          flowery::coro_return("foo")
      }

---

    Code
      set_returns(function() {
        "foo"
        "bar"
      })
    Output
      {
          "foo"
          flowery::coro_return("bar")
      }

---

    Code
      set_returns(function() { })
    Output
      {
          flowery::coro_return(invisible(NULL))
      }

---

    Code
      set_returns(function() {
        { }
        })
    Output
      {
          {
              flowery::coro_return(invisible(NULL))
          }
      }

---

    Code
      set_returns(function() {
        {
          "foo"
          "bar"
        }
      })
    Output
      {
          {
              "foo"
              flowery::coro_return("bar")
          }
      }

# explicit return is added to if else branches

    Code
      set_returns(function() if (TRUE) "foo")
    Output
      {
          if (TRUE) {
              flowery::coro_return("foo")
          }
          else {
              flowery::coro_return(invisible(NULL))
          }
      }

---

    Code
      set_returns(function() {
        if (TRUE) "foo" else "bar"
      })
    Output
      {
          if (TRUE) {
              flowery::coro_return("foo")
          }
          else {
              flowery::coro_return("bar")
          }
      }

---

    Code
      set_returns(function() {
        "before"
        if (TRUE) if (TRUE) "foo" else "bar" else "baz"
      })
    Output
      {
          "before"
          if (TRUE) {
              if (TRUE) {
                  flowery::coro_return("foo")
              }
              else {
                  flowery::coro_return("bar")
              }
          }
          else {
              flowery::coro_return("baz")
          }
      }

# explicit return is added after loops

    Code
      set_returns(function() {
        "before"
        repeat "foo"
      })
    Output
      {
          "before"
          repeat {
              "foo"
              next
          }
          flowery::coro_return(invisible(NULL))
      }

---

    Code
      set_returns(function() {
        "before"
        while (TRUE) "foo"
      })
    Output
      {
          "before"
          while (TRUE) {
              "foo"
              next
          }
          flowery::coro_return(invisible(NULL))
      }

---

    Code
      set_returns(function() for (i in x) "foo")
    Output
      {
          for (i in x) {
              "foo"
              next
          }
          flowery::coro_return(invisible(NULL))
      }

# explicit returns are swapped

    Code
      set_returns(function() return("foo"))
    Output
      {
          flowery::coro_return("foo")
      }

---

    Code
      set_returns(function() {
        "foo"
        return("bar")
      })
    Output
      {
          "foo"
          flowery::coro_return("bar")
      }

---

    Code
      set_returns(function() list("foo"))
    Output
      {
          flowery::coro_return(list("foo"))
      }

# invisible return is added after trailing yield()

    Code
      set_returns(function() yield())
    Output
      {
          yield()
          flowery::coro_return(invisible(NULL))
      }

---

    Code
      set_returns(function() if (TRUE) yield())
    Output
      {
          if (TRUE) {
              yield()
              flowery::coro_return(invisible(NULL))
          }
          else {
              flowery::coro_return(invisible(NULL))
          }
      }

