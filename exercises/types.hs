--recursive types

--defining recursive natural numbers
data Nat = Zero | Succ Nat

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

addNat :: Nat -> Nat -> Nat
addNat Zero     m = m
addNat (Succ n) m = Succ (addNat n m)

--for testing: nat2int (addNat (int2nat 10) (int2nat 11))

--exercise 1
mulNat :: Nat -> Nat -> Nat
mulNat Zero _ = Zero
mulNat _ Zero = Zero
mulNat m (Succ (Zero)) = m
mulNat (Succ (Zero)) m = m
mulNat (Succ n) m = addNat m (mulNat n m)

--exercise 3

data Tree a = Leaf a | Node (Tree a) (Tree a)

leaves :: (Tree a) -> Int
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

--balanced tree: (Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4)))
--unbalanced tree: (Node (Node (Node (Node (Leaf 4) (Leaf 5)) (Leaf 3)) (Leaf 2)) (Node (Leaf 3) (Leaf 4)))

balanced :: (Tree a) -> Bool
balanced (Leaf _) = True
balanced (Node l r) = abs (leaves l - leaves r) <= 1
                    && balanced l && balanced r

--exercise 4

halve :: [a] -> ([a], [a])
halve x = (take half_size x, drop half_size x)
    where
        half_size = div (length x) 2

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance (fst (halve xs))) (balance (snd (halve xs)))

--exercise 5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y) 

--tests:
--folde (\x -> x) (\x y -> x + y) (Add (Val 3) (Val 4))
--folde (\x -> x) (\x y -> x + y) (Add (Add (Val 3) (Val 4)) (Add (Val 2) (Val 0)))

--exercise 6

size :: Expr -> Int
size (Val x) = 1
size (Add x y) = size x + size y 















