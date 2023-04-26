import Data.Char

--ex 3 redefine map f and filter p using foldr

map_fldr :: (a -> c) -> [a] -> [c]
map_fldr f = foldr (\x -> (\y -> (f x):y)) []

filter_fldr :: (a -> Bool) -> [a] -> [a]
filter_fldr f = foldr (\x -> case (f x) of 
                        True -> (\y -> x:y)
                        _ -> (\y -> y)) []


--ex 4 convert a decimal number [1,2,3,4] to 1234 using foldl
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

--ex 5
curry :: ((x,y) -> a) -> (x -> y -> a)
curry f = \x y -> f (x, y)

uncurry :: (x -> (y -> c)) -> ((x,y) -> c)
uncurry f = \(x,y) -> f x y

--ex 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 l = auxAltMap f1 f2 True l

auxAltMap :: (a -> b) -> (a -> b) -> Bool -> [a] -> [b]
auxAltMap _  _  _ [] = []
auxAltMap f1 f2 True (x:xs) = (f1 x):(auxAltMap f1 f2 False xs)
auxAltMap f1 f2 False (x:xs) = (f2 x):(auxAltMap f1 f2 True xs)

--another solution
altMap2 :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap2 f g []       = []
altMap2 f g (x:[])   = f x : []
altMap2 f g (x:y:xs) = f x : g y : altMap2 f g xs

--ex 7 parity checker

int2bin :: Int -> [Int]
int2bin 0 = []
int2bin i = mod i 2 : int2bin (div i 2)

make8 :: [Int] -> [Int]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Int]
encode = concat . map (addParity . make8 . int2bin . ord)

addParity :: [Int] -> [Int]
addParity x | odd count = x ++ [1]
            | otherwise = x ++ [0]
            where
                count = (length (filter (==1) x))

bin2int :: [Int] -> Int
bin2int = sum . foldr (\x y -> (x*2^(length y)):y) [] . reverse

chop9 :: [Int] -> [[Int]]
chop9 [] = []
chop9 x = (take 9 x):chop9 (drop 9 x)  

decode :: [Int] -> String
decode = map (chr . bin2int . checkParity) . chop9

checkParity :: [Int] -> [Int]
checkParity x | odd (length (filter (==1) info)) && (parityBit == [1]) = info
              | even (length (filter (==1) info)) && (parityBit == [0]) = info
              | otherwise = error "parity error"
              where
                info = take 8 x
                parityBit = drop 8 x
