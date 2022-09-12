guard x | (x == 0) = 0
        | (x == 1) = 1
        | otherwise = 20

my_and :: Bool -> Bool -> Bool
my_and False _ = False
my_and _ False = False
my_and True True = True