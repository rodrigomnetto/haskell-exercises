--class constraint example (allow only types that belong to the "Ord" set)
constraint_ex :: (Ord a) => a -> a -> Bool
constraint_ex a b = a > b

const2 :: a -> b -> a
const2 x _ = x

const3 :: a -> (b -> a)
const3 x = (\_ -> x)

halve1 :: [a] -> ([a], [a])
halve1 list | null list = ([], [])
           | (mod (length list) 2) /= 0 = ([], [])
           | otherwise = internal_halve1 list []

internal_halve1 :: [a] -> [a] -> ([a], [a])
internal_halve1 (x:xs) second_half | (length (x:xs)) == (length second_half) = (second_half, (x:xs))
                                  | otherwise = internal_halve1 xs (x:second_half)

halve2 :: [a] -> ([a], [a])
halve2 list = ((take half_size list), reverse (take half_size (reverse list)))
    where 
        half_size = div (length list) 2