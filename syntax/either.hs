import Data.Either

either_test :: (a -> c) -> (b -> c) -> Either a b -> c
either_test lf _ (Left j) = (lf j)
either_test _ rf (Right j) = (rf j)

--using the either built-in function:
either_test_2 :: (a -> c) -> (b -> c) -> Either a b -> c
either_test_2 lf rf j = either lf rf j

--test cases
--either_test_2 (\x -> "hi " ++ x) (\y -> "bye " ++ y) (Left "John")  prints: hi John
--either_test_2 (\x -> "hi " ++ x) (\y -> "bye " ++ y) (Right "John")  prints: bye John

--https://www.youtube.com/watch?v=IgdZX5wav1Q