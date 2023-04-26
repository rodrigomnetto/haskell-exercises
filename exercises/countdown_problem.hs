-- 2 -> [3] -> [[2,3] [3,2]]

insert :: a -> [a] -> [[a]]
insert x [] = [[x]]
insert x (y:ys) = (x:y:ys) : (map (y:) (insert x ys))

--[[1], [2], [3]]
--[[[1],[2],[3]] , [[4],[5],[6]]]

--test list syntax
test x = [k | y <- x, k <- y]

--exercise 2

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x:_) [] = False
isChoice x (y:ys) = (isChoice (remove y x) ys)

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove y (x:xs) | x == y = xs
                | otherwise = x:(remove y xs)

