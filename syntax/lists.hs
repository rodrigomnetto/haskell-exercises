get_size [] = 0
get_size (x:xs) = get_size(xs) + 1

--comparing two lists
equal_elements ([], []) = True
equal_elements ((x:xs), (y:ys)) | (x /= y) = False
                                | (equal_elements (xs, ys) == False) = False
                                | otherwise = True

equal_lists (x, y) | (get_size x /= get_size y) = False
                   | (equal_elements (x, y) == False) = False 
                   | otherwise = True


--better solution for comparing two lists
equal_lists2 ([], []) = True
equal_lists2 ([], _) = False
equal_lists2 (_, []) = False
equal_lists2 ((x:xs), (y:ys)) | (x == y) = equal_lists2 (xs, ys)
                              | otherwise = False

--return inverse list [1, 2, 3] -> [3, 2, 1]
revert :: [t] -> [t]
revert a = revert_aux a []

revert_aux :: [t] -> [t] -> [t]
revert_aux [] a = a
revert_aux (x:xs) b = revert_aux xs (x:b)

--or concatenating lists:
revert2 :: [t] -> [t]
revert2 [] = []
revert2 (x:xs) = revert2 xs ++ [x]
--https://stackoverflow.com/questions/53123008/correct-way-to-add-an-element-to-the-end-of-a-list

