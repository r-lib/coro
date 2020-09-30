# `repeat` - one pause

    Code
      machine_parts(function() {
        "before"
        repeat {
          "loop-before"
          yield(1L)
          "loop-after"
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
          "loop-before"
          flowery::coro_yield("3", 1L)
      }
      
      [[3]]
      {
          "loop-after"
          flowery::coro_goto("2")
      }
      
      [[4]]
      {
          flowery::coro_return("after")
      }
      

# `repeat` - no continuation

    Code
      machine_parts(function() {
        "before"
        repeat yield(1L)
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
          flowery::coro_yield("3", 1L)
      }
      
      [[3]]
      {
          flowery::coro_goto("2")
      }
      
      [[4]]
      {
          flowery::coro_return("after")
      }
      

---

    Code
      machine_parts(function() {
        "before"
        repeat {
          {
            yield(1L)
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
          flowery::coro_yield("3", 1L)
      }
      
      [[3]]
      {
          flowery::coro_goto("2")
      }
      
      [[4]]
      {
          flowery::coro_return("after")
      }
      

# `repeat` - pause within `if`

    Code
      machine_parts(function() {
        "before"
        repeat {
          "loop-before"
          if (TRUE) yield(1L)
          "loop-after"
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
          "loop-before"
          if (TRUE) {
              flowery::coro_yield("3", 1L)
          }
          flowery::coro_goto("3")
      }
      
      [[3]]
      {
          "loop-after"
          flowery::coro_goto("2")
      }
      
      [[4]]
      {
          flowery::coro_return("after")
      }
      

# `repeat` - nested loop

    Code
      machine_parts(function() {
        "before"
        repeat {
          "loop-before"
          repeat yield(1L)
          "loop-after"
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
          "loop-before"
          flowery::coro_goto("3")
      }
      
      [[3]]
      {
          flowery::coro_yield("3", 1L)
      }
      
      [[4]]
      {
          "loop-after"
          flowery::coro_goto("2")
      }
      
      [[5]]
      {
          flowery::coro_return("after")
      }
      

# `repeat` - non-yielding

    Code
      machine_parts(function() {
        "before"
        repeat NULL
        yield(1L)
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
          NULL
          flowery::coro_goto("2")
      }
      
      [[3]]
      {
          flowery::coro_yield("4", 1L)
      }
      
      [[4]]
      {
          flowery::coro_return("after")
      }
      

# `repeat` - non-yielding but other control flow constructs

    Code
      machine_parts(function() {
        "before"
        repeat if (TRUE) `break`() else `next`()
        yield(1L)
        "after"
      })
    Output
      [[1]]
      {
          "before"
          flowery::coro_goto("2")
      }
      
      [[2]]
      if (TRUE) {
          flowery::coro_goto("4")
      } else {
          flowery::coro_goto("2")
      }
      
      [[3]]
      {
          flowery::coro_goto("2")
      }
      
      [[4]]
      {
          flowery::coro_yield("5", 1L)
      }
      
      [[5]]
      {
          flowery::coro_return("after")
      }
      

# loops - single `next`

    Code
      machine_parts(function() {
        repeat {
          `next`()
          yield(1L)
        }
      })
    Output
      [[1]]
      {
          flowery::coro_goto("2")
      }
      
      [[2]]
      {
          flowery::coro_goto("2")
      }
      
      [[3]]
      {
          flowery::coro_yield("4", 1L)
      }
      
      [[4]]
      {
          flowery::coro_goto("2")
      }
      
      [[5]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

# loops - single `next` with past and future

    Code
      machine_parts(function() {
        repeat {
          "loop-before"
          yield(1L)
          "loop-after"
          `next`()
          "next-after"
          yield(2L)
          "loop-final"
        }
      })
    Output
      [[1]]
      {
          flowery::coro_goto("2")
      }
      
      [[2]]
      {
          "loop-before"
          flowery::coro_yield("3", 1L)
      }
      
      [[3]]
      {
          "loop-after"
          flowery::coro_goto("2")
      }
      
      [[4]]
      {
          "next-after"
          flowery::coro_yield("5", 2L)
      }
      
      [[5]]
      {
          "loop-final"
          flowery::coro_goto("2")
      }
      
      [[6]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

# loops - single `break`

    Code
      machine_parts(function() {
        repeat {
          `break`()
          yield(1L)
        }
      })
    Output
      [[1]]
      {
          flowery::coro_goto("2")
      }
      
      [[2]]
      {
          flowery::coro_goto("5")
      }
      
      [[3]]
      {
          flowery::coro_yield("4", 1L)
      }
      
      [[4]]
      {
          flowery::coro_goto("2")
      }
      
      [[5]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

# loops - `next` and `break` within `if`-`else`

    Code
      machine_parts(function() {
        repeat {
          "loop-after"
          if (TRUE) `break`() else `next`()
          "next-after"
        }
      })
    Output
      [[1]]
      {
          flowery::coro_goto("2")
      }
      
      [[2]]
      {
          "loop-after"
          if (TRUE) {
              flowery::coro_goto("4")
          }
          else {
              flowery::coro_goto("2")
          }
      }
      
      [[3]]
      {
          "next-after"
          flowery::coro_goto("2")
      }
      
      [[4]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

# loops - `break` and `next` with past and future

    Code
      machine_parts(function() {
        repeat {
          "loop-before"
          yield(1L)
          "loop-after"
          `break`()
          "break-after"
          `next`()
          "next-after"
          yield(2L)
          "loop-final"
        }
      })
    Output
      [[1]]
      {
          flowery::coro_goto("2")
      }
      
      [[2]]
      {
          "loop-before"
          flowery::coro_yield("3", 1L)
      }
      
      [[3]]
      {
          "loop-after"
          flowery::coro_goto("7")
      }
      
      [[4]]
      {
          "break-after"
          flowery::coro_goto("2")
      }
      
      [[5]]
      {
          "next-after"
          flowery::coro_yield("6", 2L)
      }
      
      [[6]]
      {
          "loop-final"
          flowery::coro_goto("2")
      }
      
      [[7]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

# loops - goto loop start after `if` or `else`

    Code
      machine_parts(function() {
        repeat if (TRUE) yield()
      })
    Output
      [[1]]
      {
          flowery::coro_goto("2")
      }
      
      [[2]]
      {
          if (TRUE) {
              flowery::coro_yield("3")
          }
          flowery::coro_goto("3")
      }
      
      [[3]]
      {
          flowery::coro_goto("2")
      }
      
      [[4]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

---

    Code
      machine_parts(function() {
        repeat if (TRUE) yield(1L) else FALSE
      })
    Output
      [[1]]
      {
          flowery::coro_goto("2")
      }
      
      [[2]]
      {
          if (TRUE) {
              flowery::coro_yield("3", 1L)
          }
          else FALSE
          flowery::coro_goto("3")
      }
      
      [[3]]
      {
          flowery::coro_goto("2")
      }
      
      [[4]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

# `while` - single pause no past or future

    Code
      machine_parts(function() {
        while (TRUE) yield(1L)
      })
    Output
      [[1]]
      {
          if (TRUE) {
              flowery::coro_goto("2")
          }
          else {
              flowery::coro_goto("4")
          }
      }
      
      [[2]]
      {
          flowery::coro_yield("3", 1L)
      }
      
      [[3]]
      {
          flowery::coro_goto("1")
      }
      
      [[4]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

# `while` - pause within `if`

    Code
      machine_parts(function() {
        while (TRUE) {
          if (FALSE) yield(1L)
        }
      })
    Output
      [[1]]
      {
          if (TRUE) {
              flowery::coro_goto("2")
          }
          else {
              flowery::coro_goto("4")
          }
      }
      
      [[2]]
      {
          if (FALSE) {
              flowery::coro_yield("3", 1L)
          }
          flowery::coro_goto("3")
      }
      
      [[3]]
      {
          flowery::coro_goto("1")
      }
      
      [[4]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

# `while` - pause within `if` with future

    Code
      machine_parts(function() {
        while (TRUE) {
          if (FALSE) {
            yield(1L)
            "after-pause"
          }
        }
      })
    Output
      [[1]]
      {
          if (TRUE) {
              flowery::coro_goto("2")
          }
          else {
              flowery::coro_goto("5")
          }
      }
      
      [[2]]
      {
          if (FALSE) {
              flowery::coro_yield("3", 1L)
          }
          flowery::coro_goto("4")
      }
      
      [[3]]
      {
          "after-pause"
          flowery::coro_goto("4")
      }
      
      [[4]]
      {
          flowery::coro_goto("1")
      }
      
      [[5]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

# `while` - past before loop

    Code
      machine_parts(function() {
        "before"
        while (TRUE) {
          "loop-before"
          yield(1L)
          "loop-after"
        }
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
              flowery::coro_goto("5")
          }
      }
      
      [[3]]
      {
          "loop-before"
          flowery::coro_yield("4", 1L)
      }
      
      [[4]]
      {
          "loop-after"
          flowery::coro_goto("2")
      }
      
      [[5]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

# `while` - pause after loop

    Code
      machine_parts(function() {
        while (TRUE) {
          yield(1L)
          "loop-after"
        }
        yield(2L)
      })
    Output
      [[1]]
      {
          if (TRUE) {
              flowery::coro_goto("2")
          }
          else {
              flowery::coro_goto("4")
          }
      }
      
      [[2]]
      {
          flowery::coro_yield("3", 1L)
      }
      
      [[3]]
      {
          "loop-after"
          flowery::coro_goto("1")
      }
      
      [[4]]
      {
          flowery::coro_yield("5", 2L)
      }
      
      [[5]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

# `while` - complex control flow

    Code
      machine_parts(function() {
        "before"
        while (TRUE) `break`()
        while (TRUE) {
          "loop-before"
          yield(1L)
          "loop-after"
          if (TRUE) {
            "break-before"
            `break`()
            "break-after"
          } else {
            "yield-2-before"
            yield(2L)
            "yield-2-after"
          }
          "next-before"
          `next`()
          "loop-end"
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
              flowery::coro_goto("5")
          }
      }
      
      [[3]]
      {
          flowery::coro_goto("5")
      }
      
      [[4]]
      {
          flowery::coro_goto("2")
      }
      
      [[5]]
      {
          if (TRUE) {
              flowery::coro_goto("6")
          }
          else {
              flowery::coro_goto("12")
          }
      }
      
      [[6]]
      {
          "loop-before"
          flowery::coro_yield("7", 1L)
      }
      
      [[7]]
      {
          "loop-after"
          if (TRUE) {
              "break-before"
              flowery::coro_goto("12")
          }
          else {
              "yield-2-before"
              flowery::coro_yield("9", 2L)
          }
      }
      
      [[8]]
      {
          "break-after"
          flowery::coro_goto("10")
      }
      
      [[9]]
      {
          "yield-2-after"
          flowery::coro_goto("10")
      }
      
      [[10]]
      {
          "next-before"
          flowery::coro_goto("5")
      }
      
      [[11]]
      {
          "loop-end"
          flowery::coro_goto("5")
      }
      
      [[12]]
      {
          flowery::coro_return("after")
      }
      

# `while` - top level break

    Code
      machine_parts(function() {
        while (TRUE) {
          "before-break"
          `break`()
        }
      })
    Output
      [[1]]
      {
          if (TRUE) {
              flowery::coro_goto("2")
          }
          else {
              flowery::coro_goto("4")
          }
      }
      
      [[2]]
      {
          "before-break"
          flowery::coro_goto("4")
      }
      
      [[3]]
      {
          flowery::coro_goto("1")
      }
      
      [[4]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

# `for` - top level break (#7)

    Code
      machine_parts(function() {
        for (i in x) `break`()
      })
    Output
      [[1]]
      {
          `_for_iter_2` <- x
          if (base::is.factor(`_for_iter_2`)) {
              `_for_iter_2` <- base::as.character(`_for_iter_2`)
          }
          `_for_iter_2` <- flowery::as_iterator(`_for_iter_2`)
          flowery::coro_goto("2")
      }
      attr(,"spliceable")
      [1] TRUE
      
      [[2]]
      {
          if (flowery::coro_advance(quote(i), `_for_iter_2`)) {
              flowery::coro_goto("3")
          }
          else {
              flowery::coro_goto("5")
          }
      }
      
      [[3]]
      {
          flowery::coro_goto("5")
      }
      
      [[4]]
      {
          flowery::coro_goto("2")
      }
      
      [[5]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

# `for` - one pause with no past or future

    Code
      machine_parts(function() {
        for (i in x) yield(1L)
      })
    Output
      [[1]]
      {
          `_for_iter_2` <- x
          if (base::is.factor(`_for_iter_2`)) {
              `_for_iter_2` <- base::as.character(`_for_iter_2`)
          }
          `_for_iter_2` <- flowery::as_iterator(`_for_iter_2`)
          flowery::coro_goto("2")
      }
      attr(,"spliceable")
      [1] TRUE
      
      [[2]]
      {
          if (flowery::coro_advance(quote(i), `_for_iter_2`)) {
              flowery::coro_goto("3")
          }
          else {
              flowery::coro_goto("5")
          }
      }
      
      [[3]]
      {
          flowery::coro_yield("4", 1L)
      }
      
      [[4]]
      {
          flowery::coro_goto("2")
      }
      
      [[5]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

# `for` - one pause with past and future

    Code
      machine_parts(function() {
        "before"
        for (i in x) {
          "for-before"
          yield(1L)
          "for-after"
        }
        "after"
      })
    Output
      [[1]]
      {
          "before"
          `_for_iter_2` <- x
          if (base::is.factor(`_for_iter_2`)) {
              `_for_iter_2` <- base::as.character(`_for_iter_2`)
          }
          `_for_iter_2` <- flowery::as_iterator(`_for_iter_2`)
          flowery::coro_goto("2")
      }
      
      [[2]]
      {
          if (flowery::coro_advance(quote(i), `_for_iter_2`)) {
              flowery::coro_goto("3")
          }
          else {
              flowery::coro_goto("5")
          }
      }
      
      [[3]]
      {
          "for-before"
          flowery::coro_yield("4", 1L)
      }
      
      [[4]]
      {
          "for-after"
          flowery::coro_goto("2")
      }
      
      [[5]]
      {
          flowery::coro_return("after")
      }
      

# `for` - one pause within `if` and one `break` within `else`

    Code
      machine_parts(function() {
        for (i in x) {
          "for-before"
          if (TRUE) yield(1L) else `break`()
          "if-after"
          `next`()
          "for-after"
        }
      })
    Output
      [[1]]
      {
          `_for_iter_2` <- x
          if (base::is.factor(`_for_iter_2`)) {
              `_for_iter_2` <- base::as.character(`_for_iter_2`)
          }
          `_for_iter_2` <- flowery::as_iterator(`_for_iter_2`)
          flowery::coro_goto("2")
      }
      attr(,"spliceable")
      [1] TRUE
      
      [[2]]
      {
          if (flowery::coro_advance(quote(i), `_for_iter_2`)) {
              flowery::coro_goto("3")
          }
          else {
              flowery::coro_goto("6")
          }
      }
      
      [[3]]
      {
          "for-before"
          if (TRUE) {
              flowery::coro_yield("4", 1L)
          }
          else {
              flowery::coro_goto("6")
          }
      }
      
      [[4]]
      {
          "if-after"
          flowery::coro_goto("2")
      }
      
      [[5]]
      {
          "for-after"
          flowery::coro_goto("2")
      }
      
      [[6]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

# `return()` deep in control flow

    Code
      machine_parts(function() {
        while (TRUE) if (TRUE) return(1L) else yield(2L)
      })
    Output
      [[1]]
      {
          if (TRUE) {
              flowery::coro_goto("2")
          }
          else {
              flowery::coro_goto("4")
          }
      }
      
      [[2]]
      if (TRUE) flowery::coro_return(1L) else {
          flowery::coro_yield("3", 2L)
      }
      
      [[3]]
      {
          flowery::coro_goto("1")
      }
      
      [[4]]
      {
          flowery::coro_return(invisible(NULL))
      }
      

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
      

