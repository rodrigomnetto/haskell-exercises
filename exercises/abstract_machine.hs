-- (Add (Add (Val 1) (Val 2)) (Val 4))

data Expr = Val Int | Add Expr Expr | Mul Expr Expr

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y 

--abstract machine ex 9
data Op = EVALADD Expr | EVALMUL Expr | ADD Int | MUL Int
type Stack = [Op]

eval :: Expr -> Stack -> Int
eval (Val n)   s  = exec n s
eval (Add x y) s  = eval x ((EVALADD y):s)
eval (Mul x y) s  = eval x ((EVALMUL y):s) 

exec :: Int -> Stack -> Int
exec n [] = n
exec n ((EVALADD y):s) = eval y ((ADD n):s)
exec n ((EVALMUL y):s) = eval y ((MUL n):s)
exec m ((ADD n):s)  = exec (n+m) s 
exec m ((MUL n):s)  = exec (n*m) s

