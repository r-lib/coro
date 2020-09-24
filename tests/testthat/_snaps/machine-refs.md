# references are propagated with yield

    Code
      print_parts_refs(function() {
        yield(1)
        2
      })
    Output
      <Part 1>
      [[1]]
      [[1]][[1]]
      expr: flowery::coro_yield("2", 1)
      ref: yield(1)
      
      
      <Part 2>
      [[1]]
      [[1]][[1]]
      expr: flowery::coro_return(2)
      ref: 2
      
      

---

    Code
      print_parts_refs(function() {
        1
        yield(2)
        3
      })
    Output
      <Part 1>
      [[1]]
      [[1]][[1]]
      expr: 1
      ref: 1
      
      [[1]][[2]]
      expr: flowery::coro_yield("2", 2)
      ref: yield(2)
      
      
      <Part 2>
      [[1]]
      [[1]][[1]]
      expr: flowery::coro_return(3)
      ref: 3
      
      

# references are propagated in repeat loops

    Code
      print_parts_refs(function() {
        repeat yield(1)
      })
    Output
      <Part 1>
      [[1]]
      [[1]][[1]]
      expr: flowery::coro_goto("2")
      ref: repeat yield(1)
      
      
      <Part 2>
      [[1]]
      [[1]][[1]]
      expr: flowery::coro_yield("2", 1)
      ref: NULL
      
      
      <Part 3>
      [[1]]
      [[1]][[1]]
      expr: flowery::coro_return(invisible(NULL))
      ref: NULL
      
      

---

    Code
      print_parts_refs(function() {
        repeat {
          1
          `break`()
          3
        }
      })
    Output
      <Part 1>
      [[1]]
      [[1]][[1]]
      expr: flowery::coro_goto("2")
      ref: repeat {
          1
          `break`()
          3
        }
      
      
      <Part 2>
      [[1]]
      [[1]][[1]]
      expr: 1
      ref: 1
      
      [[1]][[2]]
      expr: flowery::coro_goto("4")
      ref: `break`()
      
      
      <Part 3>
      [[1]]
      [[1]][[1]]
      expr: 3
      ref: 3
      
      
      <Part 4>
      [[1]]
      [[1]][[1]]
      expr: flowery::coro_return(invisible(NULL))
      ref: NULL
      
      

# references are propagated in while loops

    Code
      print_parts_refs(function() {
        while (TRUE) `next`()
        while (TRUE) `break`()
      })
    Output
      <Part 1>
      [[1]]
      [[1]][[1]]
      expr: TRUE
      ref: while (TRUE) `next`()
      
      
      <Part 3>
      [[1]]
      [[1]][[1]]
      expr: TRUE
      ref: while (TRUE) `break`()
      
      
      <Part 5>
      [[1]]
      [[1]][[1]]
      expr: flowery::coro_return(invisible(NULL))
      ref: NULL
      
      

---

    Code
      print_parts_refs(function() {
        1
        while (TRUE) {
          2
          `next`()
          3
          `break`()
          4
        }
        5
      })
    Output
      <Part 1>
      [[1]]
      [[1]][[1]]
      expr: 1
      ref: 1
      
      
      <Part 2>
      [[1]]
      [[1]][[1]]
      expr: TRUE
      ref: while (TRUE) {
          2
          `next`()
          3
          `break`()
          4
        }
      
      
      <Part 3>
      [[1]]
      [[1]][[1]]
      expr: 2
      ref: 2
      
      [[1]][[2]]
      expr: flowery::coro_goto("2")
      ref: `next`()
      
      
      <Part 4>
      [[1]]
      [[1]][[1]]
      expr: 3
      ref: 3
      
      [[1]][[2]]
      expr: flowery::coro_goto("6")
      ref: `break`()
      
      
      <Part 5>
      [[1]]
      [[1]][[1]]
      expr: 4
      ref: 4
      
      
      <Part 6>
      [[1]]
      [[1]][[1]]
      expr: flowery::coro_return(5)
      ref: 5
      
      

