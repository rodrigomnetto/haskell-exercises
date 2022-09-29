type Age = Int
type Name = String
type City = String
type Language = String
type University = String
type Country = String
type Person1 = (Name, Age)

get_person :: Person1
get_person = ("Mike", 22)

get_age :: Person1 -> Int
get_age (_, a) = a

get_name :: Person1 -> String
get_name (a, _) = a

--other types Float, Double

--algebraic types
data Person = Programmer Name Language | Student Name University
    deriving(Show)

programmer = Programmer "James" "c#"
student = Student "Mark" "MIT"

is_programmer :: Person -> Bool
is_programmer (Programmer _ _) = True
is_programmer _ = False

--link about typeclasses
--http://learnyouahaskell.com/types-and-typeclasses

--link about haskell type system
--https://mmhaskell.com/blog/2016/12/5/7mkljzq7zy97d66zm4yvtn8v1ph502#:~:text=Haskell%20is%20a%20statically%20typed,top%20level%20functions%20and%20expressions.

