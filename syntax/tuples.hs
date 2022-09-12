tuple_example :: (Int, Int) -> (Int, Int) -> (Int, Int)
tuple_example (a, b) (c, d) = (a+c, b+d)

tuple_2 :: (String, String) -> (Int) -> (String)
tuple_2 (a, b) val | (val == 0) = fst (a, b)
                    | otherwise = snd (a, b)

names :: (String, String, String)
names = ("James", "Eric", "Joshua")

first_name :: (String, String, String) -> String
first_name (a, _, _) = a

third_name :: (String, String, String) -> String
third_name (_, _, c) = c