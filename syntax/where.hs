quad :: Int -> Int
quad n = quad_n where quad_n = n*n --bind the function (n*n) to quad_n. quad_n is only evaluated when called in (quad n = quad_n)
                                -- "where" only binds a function to a name, but doesn't evaluate the function.

--https://wiki.haskell.org/Let_vs._Where
--https://www.youtube.com/watch?v=pitjnqRKyyI