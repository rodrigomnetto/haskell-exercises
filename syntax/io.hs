main :: IO()
main = do
    putStr "Type the first number: "
    n1 <- getLine
    putStr "Type the second number: "
    n2 <- getLine
    putStrLn ("Sum: " ++ (show (read n1 + read n2))) --read: string to number, show: print string