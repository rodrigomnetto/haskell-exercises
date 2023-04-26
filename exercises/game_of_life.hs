import Control.Concurrent

type Pos = (Int,Int)
type Board = [Pos]

width = 10
height = 10

clean :: IO ()
clean =  do putStr "\ESC[2J"       

printBoard :: Board -> IO ()
printBoard b = sequence_ [writeAt x | x <- b]

writeAt :: Pos -> IO ()
writeAt (x,y) = do 
            putStr ("\ESC[" ++ show x ++ ";" ++ show y ++ "H")
            putChar 'o'

--the game starts here
main :: IO ()
main = do life [(5,4),(5,5),(5,6)]
--main = do life [(4,2),(2,3),(4,3),(3,4),(4,4)]

life :: Board -> IO ()
life b = do (clean)
            (printBoard b)
            threadDelay 500000 
            life (updateBoard b)

wrap :: Pos -> Pos
wrap (x,y) = ((mod (x-1) width) + 1, (mod (y-1) height) + 1)

neighbours :: Pos -> [Pos]
neighbours (x,y) = map wrap [(x - 1, y-1),(x-1, y)
                            ,(x-1, y+1),(x, y-1)
                            ,(x, y+1),(x+1, y-1)
                            ,(x+1, y),(x+1, y+1)]

isAlive :: Board -> Pos -> Int
isAlive b p = if (elem p b) then 1 else 0

shouldSurvive :: Board -> Pos -> Bool
shouldSurvive b p = living == 2 || living == 3 
    where living = (sum (map (isAlive b) (neighbours p)))

shouldBirth :: Board -> Pos -> Bool
shouldBirth b p = (sum (map (isAlive b) (neighbours p))) == 3

trav :: [Pos] -> Board -> Board
trav [] _ = []
trav (x:xs) b | (elem x b) = if shouldSurvive b x then x:(trav xs b)
                                                            else trav xs b
                        | shouldBirth b x = x:(trav xs b)
                        | otherwise = trav xs b 

updateBoard :: Board -> Board
updateBoard b = trav [(x,y) | x <- [1..width], y <- [1..height]] b