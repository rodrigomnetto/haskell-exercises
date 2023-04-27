--functors examples

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe a = fmap (+ 1) a

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

instance Functor Tree where
    fmap f (Node l r) = Node (fmap f l) (fmap f r)
    fmap f (Leaf a) = Leaf (f a)    

incTree :: Num a => Tree a -> Tree a
incTree t = fmap (+1) t

data Perhaps a = Empty | Full a
    deriving Show

instance Functor Perhaps where
    fmap _ Empty = Empty
    fmap f (Full a) = (Full (f a))

incPerhaps :: Num a => Perhaps a -> Perhaps a
incPerhaps x = fmap (+1) x 

--applicatives

sumThreeNumbers :: Num a => a -> a -> a -> a
sumThreeNumbers x y z = (x + y + z)

sumApplicativeStyle :: Num a => a -> a -> a -> Maybe a
sumApplicativeStyle x y z = pure sumThreeNumbers <*> Just x <*> Just y <*> Just z

{-
instance Applicative Perhaps where
    pure a = Full a
    Empty <*> _ = Empty
    (Full f) <*> Empty = Empty
    (Full f) <*> (Full x) = Full (f x)
    -}

--or, using Functor fmap
instance Applicative Perhaps where
    pure = Full
    Empty <*> _ = Empty
    (Full f) <*> x = fmap f x

--applicative example
getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n-1)

--define fake IO type called IO2
data IO2 a = Return a
    deriving Show

instance Functor IO2 where
    fmap f (Return a) = Return (f a)

instance Applicative IO2 where
    pure = Return
    (Return a) <*> x = fmap a x 

fakeGetChars :: Int -> IO2 String
fakeGetChars 0 = Return []
fakeGetChars n = pure (:) <*> (sillyChars n) <*> fakeGetChars (n-1)

sillyChars :: Int -> IO2 Char
sillyChars n | ((mod n 2) == 0) = Return 'a'
             | otherwise = Return 'b'

--option data type (F#)
data Option a = None | Some a
    deriving Show

instance Functor Option where
    fmap _ None = None
    fmap f (Some a) = Some (f a)

instance Applicative Option where
    pure = Some
    None <*> _ = None
    (Some f) <*> a = fmap f a

--ex:
--pure (\x y -> x + y) <*> Some 2 <*> Some 3

--sequenceA function definition:

sequenceB :: Applicative f => [f a] -> f [a]
sequenceB [] = pure []
sequenceB (h:x) = pure (:) <*> h <*> sequenceB x

--ex: sequenceB ((Some 1):(Some 2):(Some 4):[])

getChars3 :: Int -> IO String
getChars3 n = sequenceB (replicate n getChar)

--monads:

--maybe monad

returnJustOrNothing :: Int -> Maybe Int
returnJustOrNothing n | n > 5 = Nothing
                      | otherwise = Just n

--Just 1 >>= returnJustOrNothing  returns Just 1
--Just 6 >>= returnJustOrNothing  returns Nothing

sumMaybe :: Int -> Int -> Maybe Int
sumMaybe a b = return a >>= (
    \a -> return b >>= (\b ->  Just (a + b) ))

sumMaybeWithDo :: Int -> Int -> Maybe Int
sumMaybeWithDo a b = do n <- return a
                        m <- return b
                        Just (n + m)

--state monad

type State = Int

rlabel :: Tree Char -> State -> (Tree Int, State)
rlabel (Leaf _) n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
    where
        (l', n') = rlabel l n
        (r', n'') = rlabel r n' 


apply :: ST a -> State -> (a, State)
apply (S f) st = f st

newtype ST a = S (State -> (a, State))

instance Functor ST where
    fmap f st = S (\s -> let (x, s') = (apply st s) in (f x, s'))

instance Applicative ST where
    pure x = S (\s -> (x, s))
    stf <*> sta = S (\s -> 
        let (f, s') = apply stf s
            (x, s'') = apply sta s' in (f x, s''))

fresh :: ST Int
fresh = S (\s -> (s, s+1))

--applicative style
alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

--ex: apply (alabel (Node (Leaf 'a') (Leaf 'b'))) 0
--result: (Node (Leaf 0) (Leaf 1),2)

--monadic style

instance Monad ST where
    return = pure
    -- ST a -> (a -> ST b) -> ST b
    st >>= f = 
        S (\s -> let (n, s') = apply st s in apply (f n) s')

--using do notation
mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do
                n <- fresh
                return (Leaf n)
mlabel (Node l r) = do
                l' <- mlabel l
                r' <- mlabel r
                return (Node l' r')  

--using bind operator
mlabel2 :: Tree a -> ST (Tree Int)
mlabel2 (Leaf _) = fresh >>= (\n -> return (Leaf n))
mlabel2 (Node l r) = mlabel2 l >>= 
                        (\l' -> 
                            mlabel2 r >>= 
                                (\r' ->
                                    return (Node l' r')
                                )
                        )

--exercises

--2  partially applied function: a ->
--instance Functor ((->) a) where
    --fmap :: (a -> m b) -> m a -> m b
--    fmap f g = f . g

type GetIntReturnA a = ((->) Int) a


test :: GetIntReturnA a -> Int -> a 
test f i = f i

--test (\n -> n+1) 2 == 3

-- sum3elements(a -> b -> c -> d)

applicativeSum :: Num a => (a -> a -> a -> a) -> a
applicativeSum f = f <$> (\n -> n+1) <*> (\n -> n+1) <*>  (\n -> n+1) $ 5

sum4terms :: Num a => a -> a -> a -> a
sum4terms a b c = a + b + c

--applicativeSum sum4terms

--functor fmap: (a -> b) -> m a -> m b

--applicative <*>: m (a -> b) -> m a -> m b
--pure a -> m a

--monad >>=: m a -> (a -> m b) -> m b

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) s = p s 

item :: Parser Char
item = P (\inp -> case inp of
          [] -> []
          x:xs -> [(x, xs)])



instance Functor Parser where
    fmap f p = P (\n ->
        let [(c, out)] = parse p n
        in [(f c, out)])

instance Applicative Parser where
    pure a = P (\n -> [(a, n)])
    f <*> p = P (\n -> case parse f n of
        [] -> []
        [(g, out)] -> parse (fmap g p) out)

instance Monad Parser where
    return = pure
    p >>= f = P (\n -> case parse p n of 
        [] -> []
        [(v, out)] -> parse (f v) out)
        

 --receives string returns first and third char
func :: Parser (Char,Char)
func = pure g <*> item <*> item <*> item
        where g x y z = (x, z)


--example: parse func "abcd" = [(('a','c'), "d")]

--using monad example:

func2 :: Parser (Char, Char)
func2 = item >>= (\x ->
            item >>= (\_ ->
                item >>= (\z ->
                    return (x,z)
                )
            )
        )

--example: parse func2 "abcd" = [(('a','c'), "d")]

--using do syntax:

func3 :: Parser (Char, Char)
func3 = do
    x <- item
    _ <- item
    z <- item
    return (x, z)














