--number exists in list
contains :: [Int] -> Int -> Bool
contains [] _ = False
contains (x:xs) a |  (x == a) = True
                  |  otherwise = contains xs a

--biggest element in list
get_biggest :: [Int] -> Int
get_biggest [x] = x
get_biggest (x:xs) | (x > get_biggest xs) = x
                   | otherwise = get_biggest xs

--better performance solution
get_biggest2 :: [Int] -> Int
get_biggest2 [x] = x
get_biggest2 (x:xs) = get_biggest2_aux xs x

get_biggest2_aux :: [Int] -> Int -> Int
get_biggest2_aux [x] a | (a > x) = a
                       | otherwise = x
get_biggest2_aux (x:xs) a | (a > x) = get_biggest2_aux xs a
                          | otherwise = get_biggest2_aux xs x 
