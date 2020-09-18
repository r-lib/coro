# explicit return is added to blocks

    Code
      set_returns(function() {
        "foo"
      })
    Output
      [[1]]
      flowery::coro_return("foo")
      

---

    Code
      set_returns(function() {
        "foo"
        "bar"
      })
    Output
      [[1]]
      [1] "foo"
      
      [[2]]
      flowery::coro_return("bar")
      

---

    Code
      set_returns(function() { })
    Output
      [[1]]
      flowery::coro_return(NULL)
      

---

    Code
      set_returns(function() {
        { }
        })
    Output
      [[1]]
      {
          flowery::coro_return(NULL)
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
      [[1]]
      {
          "foo"
          flowery::coro_return("bar")
      }
      

# explicit return is added to if else branches

    Code
      set_returns(function() if (TRUE) "foo")
    Output
      [[1]]
      if (TRUE) {
          flowery::coro_return("foo")
      } else {
          flowery::coro_return(invisible(NULL))
      }
      

---

    Code
      set_returns(function() {
        if (TRUE) "foo" else "bar"
      })
    Output
      [[1]]
      if (TRUE) {
          flowery::coro_return("foo")
      } else {
          flowery::coro_return("bar")
      }
      

---

    Code
      set_returns(function() {
        "before"
        if (TRUE) if (TRUE) "foo" else "bar" else "baz"
      })
    Output
      [[1]]
      [1] "before"
      
      [[2]]
      if (TRUE) {
          if (TRUE) {
              flowery::coro_return("foo")
          }
          else {
              flowery::coro_return("bar")
          }
      } else {
          flowery::coro_return("baz")
      }
      

# explicit return is added after loops

    Code
      set_returns(function() {
        "before"
        repeat "foo"
      })
    Output
      [[1]]
      [1] "before"
      
      [[2]]
      repeat {
          "foo"
          next
      }
      
      [[3]]
      flowery::coro_return(invisible(NULL))
      

---

    Code
      set_returns(function() {
        "before"
        while (TRUE) "foo"
      })
    Output
      [[1]]
      [1] "before"
      
      [[2]]
      while (TRUE) {
          "foo"
          next
      }
      
      [[3]]
      flowery::coro_return(invisible(NULL))
      

---

    Code
      set_returns(function() for (i in x) "foo")
    Output
      [[1]]
      for (i in x) {
          "foo"
          next
      }
      
      [[2]]
      flowery::coro_return(invisible(NULL))
      

# explicit returns are swapped

    Code
      set_returns(function() return("foo"))
    Output
      [[1]]
      flowery::coro_return("foo")
      

---

    Code
      set_returns(function() {
        "foo"
        return("bar")
      })
    Output
      [[1]]
      [1] "foo"
      
      [[2]]
      flowery::coro_return("bar")
      

---

    Code
      set_returns(function() list("foo"))
    Output
      [[1]]
      flowery::coro_return(list("foo"))
      

# invisible return is added after trailing yield()

    Code
      set_returns(function() yield())
    Output
      [[1]]
      yield()
      
      [[2]]
      flowery::coro_return(invisible(NULL))
      

---

    Code
      set_returns(function() if (TRUE) yield())
    Output
      [[1]]
      if (TRUE) {
          yield()
          flowery::coro_return(invisible(NULL))
      } else {
          flowery::coro_return(invisible(NULL))
      }
      

