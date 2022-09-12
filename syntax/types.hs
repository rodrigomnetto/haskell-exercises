type Age = Int
type Name = String
type Person = (Name, Age)

get_person :: Person
get_person = ("Mike", 22)

get_age :: Person -> Int
get_age (_, a) = a

get_name :: Person -> String
get_name (a, _) = a