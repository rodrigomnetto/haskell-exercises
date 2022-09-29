
--example using maybe
data Expression = Val Int | Div Expression Expression

evaluate :: Expression -> Maybe Int
evaluate (Val n) = return n
evaluate (Div a b) = evaluate a >>= (\x ->
    evaluate b >>= (\y ->
            if y == 0 then Nothing
            else return (div x y)
        ))
-- a -> b   b -> c = a -> c

--rewriting with the "do" keyword (does the same, it's only syntactic sugar)
evaluate2 :: Expression -> Maybe Int
evaluate2 (Val n) = return n
evaluate2 (Div a b) = do
    k <- evaluate a
    j <- evaluate b
    if j == 0 then Nothing else return (div k j)

--creating a monad (similar to Maybe) from scratch:

--first create a monadic type:
data Option a = Some a | None deriving(Show)--dynamic type (a)

--then define the behavior of bind (>>=) and return functions:
instance Monad Option where
    return k = Some k
    m >>= f = case m of
        None -> None  
        Some j -> f j

--then implements the functor operations
instance Functor Option where -- functor apply functions to encapsulated values
    fmap f (Some a) = Some (f a)
    fmap f None = None

--then implements the applicative operations
instance Applicative Option where -- applicative apply encapsulated functions to encapsulated values
    pure a = Some a -- pure just returns a encapsulated value
    None <*> _ = None
    (Some f) <*> something = fmap f something -- calls fmap that returns Some again

--testing "Option" monadic type
evaluate3 :: Expression -> Option Int
evaluate3 (Val n) = return n
evaluate3 (Div a b) = evaluate3 a >>= (\x ->
    evaluate3 b >>= (\y ->
        if y == 0 then None else return (div x y)
    ))

--test made: evaluate3 (Div (Val 2) (Val 2))  -> returns Some 1
--test made: evaluate3 (Div (Val 2) (Val 0))  -> returns None

--more about functors and applicative:
--http://learnyouahaskell.com/functors-applicative-functors-and-monoids
--https://www.youtube.com/watch?v=xCut-QT2cpI

--in this example partial application occurs:
--pure (+) <*> Just 3 <*> Just 5   =   Just 3+  <*> Just 5
--result: Just 8



