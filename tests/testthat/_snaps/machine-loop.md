# nested loops break to outer loops

    Code
      machine_parts(function() {
        "before"
        while (TRUE) {
          "while before if"
          if (i > 3) {
            "while-if before break"
            `break`()
          }
          "while after if"
          while (TRUE) {
            "while-while before if"
            if (j > 3) {
              "while-while-if before break"
              `break`()
            }
            "while-while after if"
          }
          "while after while"
        }
        "after"
      })
    Output
      [[1]]
      {
          "before"
          flowery::coro_goto("2")
      }
      
      [[2]]
      {
          if (TRUE) {
              flowery::coro_goto("3")
          }
          else {
              flowery::coro_goto("9")
          }
      }
      
      [[3]]
      {
          "while before if"
          if (i > 3) {
              "while-if before break"
              flowery::coro_goto("9")
          }
          flowery::coro_goto("4")
      }
      
      [[4]]
      {
          "while after if"
          flowery::coro_goto("5")
      }
      
      [[5]]
      {
          if (TRUE) {
              flowery::coro_goto("6")
          }
          else {
              flowery::coro_goto("8")
          }
      }
      
      [[6]]
      {
          "while-while before if"
          if (j > 3) {
              "while-while-if before break"
              flowery::coro_goto("8")
          }
          flowery::coro_goto("7")
      }
      
      [[7]]
      {
          "while-while after if"
          flowery::coro_goto("5")
      }
      
      [[8]]
      {
          "while after while"
          flowery::coro_goto("2")
      }
      
      [[9]]
      {
          flowery::coro_return("after")
      }
      

---

    Code
      machine_parts(function() {
        "before"
        while (TRUE) {
          "while before if"
          if (i > 3) {
            "while-if before break"
            `break`()
          }
          "while after if"
          while (TRUE) {
            "while-while before if"
            if (j > 3) {
              "while-while-if before break"
              `break`()
            }
            "while-while after if"
          }
        }
        "after"
      })
    Output
      [[1]]
      {
          "before"
          flowery::coro_goto("2")
      }
      
      [[2]]
      {
          if (TRUE) {
              flowery::coro_goto("3")
          }
          else {
              flowery::coro_goto("9")
          }
      }
      
      [[3]]
      {
          "while before if"
          if (i > 3) {
              "while-if before break"
              flowery::coro_goto("9")
          }
          flowery::coro_goto("4")
      }
      
      [[4]]
      {
          "while after if"
          flowery::coro_goto("5")
      }
      
      [[5]]
      {
          if (TRUE) {
              flowery::coro_goto("6")
          }
          else {
              flowery::coro_goto("8")
          }
      }
      
      [[6]]
      {
          "while-while before if"
          if (j > 3) {
              "while-while-if before break"
              flowery::coro_goto("8")
          }
          flowery::coro_goto("7")
      }
      
      [[7]]
      {
          "while-while after if"
          flowery::coro_goto("5")
      }
      
      [[8]]
      {
          flowery::coro_goto("2")
      }
      
      [[9]]
      {
          flowery::coro_return("after")
      }
      

---

    Code
      machine_parts(function() {
        while (1) {
          while (2) {
            yield(1)
            `break`()
          }
        }
      })
    Output
      [[1]]
      {
          if (1) {
              flowery::coro_goto("2")
          }
          else {
              flowery::coro_goto("6")
          }
      }
      
      [[2]]
      {
          if (2) {
              flowery::coro_goto("3")
          }
          else {
              flowery::coro_goto("5")
          }
      }
      
      [[3]]
      {
          flowery::coro_yield("4", 1)
      }
      
      [[4]]
      {
          flowery::coro_goto("5")
      }
      
      [[5]]
      {
          flowery::coro_goto("1")
      }
      
      [[6]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

