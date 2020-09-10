# generator prints nicely

    Code
      print(zap_env(gen({
        while (TRUE) {
          if (TRUE) {
            yield(1)
          }
          return(2)
        }
      })))
    Output
      function () 
      {
          evalq(`_env`, expr = {
              while (TRUE) {
                  switch({
                      base::as.character(`_state`)
                  }, `1` = {
                      if (TRUE) {
                        flowery::coro_goto("2")
                      } else {
                        flowery::coro_goto("4")
                      }
                  }, `2` = {
                      if (TRUE) {
                        flowery::coro_yield("3", 1)
                      }
                      flowery::coro_goto("3")
                  }, `3` = {
                      flowery::coro_return(2)
                  }, `4` = {
                      flowery::coro_return(invisible(NULL))
                  }, `5` = {
                      base::return(invisible(NULL))
                  }, {
                      rlang::abort(base::sprintf("Internal error: Unexpected state `%s`.", 
                        `_state`))
                  })
              }
          })
      }

