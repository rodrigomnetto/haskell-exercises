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

--sorting list first algorithm
order :: [Int] -> [Int]
order [x] = [x]
order [] = []
order (x:xs) = order2 (order_aux xs x [])

order2 :: [Int] -> [Int]
order2 [x] = [x]
order2 (x:(k:ks)) = x:(order2 (order_aux ks k []))

order_aux :: [Int] -> Int -> [Int] -> [Int]
order_aux [] a _ = [a]
order_aux [x] a l | (a < x) = a:x:l
                       | otherwise = x:a:l
order_aux (x:xs) a l | (a < x) = order_aux xs a (x:l)
                          | otherwise = order_aux xs x (a:l)

--list in reverse order
reverse2 :: [Int] -> [Int] -> [Int]
reverse2 [x] l = x:l
reverse2 (x:xs) l = reverse2 xs (x:l)

--sorting 2.0
get_greater_number :: [Int] -> Int
get_greater_number [x] = x
get_greater_number (x:xs) = let a = get_greater_number xs
                        in if a > x then a else x

order_aux2 :: [Int] -> [Int] -> [Int]
order_aux2 [x] b = x:b 
order_aux2 a b = let greater = get_greater_number a
    in order_aux2 (filter (\e -> e /= greater) a) (greater:b)

order_list :: [Int] -> [Int]
order_list [x] = [x]
order_list a = order_aux2 a []    
    


