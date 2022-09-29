check_number :: Int -> Bool
check_number a = let number = 2 -- binds a function that returns the number 2 to the name "number". 
            in if a == number then True -- "Let" is lazy, the function (number) isn't executed until is called.
            else False

--https://wiki.haskell.org/Let_vs._Where
--https://www.youtube.com/watch?v=pitjnqRKyyI