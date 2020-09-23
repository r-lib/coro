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
      
      

# references are propagated in while loops

    Code
      print_parts_refs(function() {
        1
        while (TRUE) {
          2
          `next`()
          3
        }
        4
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
      
      
      <Part 6>
      [[1]]
      [[1]][[1]]
      expr: flowery::coro_return(4)
      ref: 4
      
      

