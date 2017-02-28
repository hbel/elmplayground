module Sets exposing (..)


type alias Set =
    Int -> Bool


singleton : Int -> Set
singleton i =
    \x -> x == i


union : Set -> Set -> Set
union s t =
    \x -> (s x) || (t x)


intersection : Set -> Set -> Set
intersection s t =
    \x -> (s x) && (t x)


difference : Set -> Set -> Set
difference s t =
    \x -> (s x) && not (t x)


contains : Int -> Set -> Bool
contains y s =
    s y
