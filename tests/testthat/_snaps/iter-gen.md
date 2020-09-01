# generator prints nicely

    Output
      function () 
      {
          evalq(env, expr = {
              while (TRUE) {
                  switch(`_state`, `1` = {
                      if (TRUE) {
                        `_goto`("2")
                      } else {
                        `_goto`("4")
                      }
                  }, `2` = {
                      if (TRUE) {
                        `_pause`("3", 1)
                      }
                      `_goto`("3")
                  }, `3` = {
                      return(2)
                  }, `4` = {
                      return(invisible(NULL))
                  }, `5` = {
                      base::return(invisible(NULL))
                  })
              }
          })
      }

