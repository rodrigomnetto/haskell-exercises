import Data.Char

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

--set theory syntax
get_numbers :: [Int]
get_numbers = [x | x <- [1, 2, 3]]

get_numbers_plus_one :: [Int]
get_numbers_plus_one = [x+1 | x <- [1, 2, 3]]

get_numbers_from_one_to_ten :: [Int]
get_numbers_from_one_to_ten = [x | x <- [1..10]]


get_multiples_of_two :: [Int]
get_multiples_of_two = [x | x <- [1..10], mod x 2 == 0]

--head - returns first element
--tail - returns list without head
--last - returns last element
--init - returns list without last element

--comprehension notation -> construct new sets from existing sets.
new_set :: [Int]
new_set = [x^2 | x <- [1..5]]

generator :: [Int]
generator = [1..5]

concat_list :: [[a]] -> [a]
concat_list x = [f | xs <- x, f <- xs]

--generate permutations between lists
permut :: [a] -> [a] -> [(a, a)]
permut a b = [(x, y) | x <- a, y <- b] 

--generate prime numbers
is_prime :: Int -> Bool
is_prime n = [x | x <- [1..n], (mod n x) == 0] == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], is_prime x]

--ceaser cipher exercise
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let i = chr (ord 'a' + i)

mods :: [Int]
mods = [mod (x+2) 26 | x <- [0..25]]

encode :: String -> Int -> String
encode chs n = [int2let (mod ((let2int x) + n) 26) | x <- chs]

decode :: String -> Int -> String
decode chs n = encode chs (negate n)

--exercises

--2
replicate2 :: Int -> a -> [a]
replicate2 n j = [j | x <- [1..n]]

--3 x^2 + y^2 = z^2

pyths :: Int -> [(Int, Int, Int)]
pyths max = [(x, y, sum x y) | x <- [1..max], y <- [1..max], elem (sum x y) sqrs]
    where 
        sum x y = (x^2) + (y^2)
        sqrs = [s^2 | s <- [1..max]]  


--7 scalar product
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct a b = sum [ x*y | (x,y) <- (zip a b)]










