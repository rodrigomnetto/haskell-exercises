div_sample :: Int -> Int -> Int
div_sample a b = div a b

mod_sample :: Int -> Int -> Int
mod_sample a b = mod a b

from_integral_sample :: Int -> Float
from_integral_sample a = (fromIntegral a) + 3.2 -- fromIntegral converts int to float

sqrt_example :: Float -> Float
sqrt_example a = sqrt a

floor_example :: Float -> Int -- 3.8 -> 3
floor_example a = floor a

ceiling_example :: Float -> Int -- 3.8 -> 4
ceiling_example a = ceiling a

round_example :: Float -> Int -- 3.6 -> 4,  3.5 -> 3
round_example a = round a