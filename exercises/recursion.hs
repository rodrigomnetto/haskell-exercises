--insertion sort

insert :: Ord a => a -> [a] -> [a]
insert y [] = y:[]
insert y (x:xs) | y < x = y:(x:xs)
                | otherwise = x:(insert y xs)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

--exercises

--ex 1
exp1 :: Int -> Int -> Int
exp1 b 0 = 1
exp1 b e = b * (exp1 b (e-1))

--ex 3

andl :: [Bool] -> Bool
andl [] = False
andl [x] = x
andl (x:xs) = x && (andl xs)  

nth_list :: [a] -> Int -> a
nth_list (x:xs) 1 = x
nth_list (x:xs) i = nth_list xs (i-1) 

--ex 4

merge :: Ord a => [a] -> [a] -> [a]
merge [] a = a
merge a [] = a
merge (x:xs) (y:ys) | x <= y = x:(merge xs (y:ys)) 
                    | otherwise = y:(merge (x:xs) ys)

--ex 5 merge sort

msort :: Ord a => [a] -> [a]
msort [j] = [j]
msort j = merge (msort first_half) (msort second_half)
    where 
        first_half = fst (halve j)
        second_half = snd (halve j)

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve a = (take half_size a, drop half_size a)
    where
        half_size = div (length a) 2

--ex 6

--take
take2 :: Int -> [a] -> [a]
take2  _ [] = []
take2 0 l = []
take2 a (x:xs) = x:(take2 (a-1) xs)

