add_one :: [Int] -> [Int]
add_one [] = []
add_one list = map (\x -> x+1) list --adds one foreach list element

add_one2 :: [Int] -> [Int]
add_one2 [] = []
add_one2 list = map (+1) list --adds one foreach list element

get_greater_then_five :: [Int] -> [Int]
get_greater_then_five [] = []
get_greater_then_five list = filter (\x -> x > 5) list --filter example


