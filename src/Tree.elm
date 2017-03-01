module Tree exposing (..)


type Tree comparable
    = Node comparable (Left comparable) (Right comparable)
    | Empty


type alias Left comparable =
    Tree comparable


type alias Right comparable =
    Tree comparable


insert : comparable -> Tree comparable -> Tree comparable
insert val t =
    case t of
        Empty ->
            Node val Empty Empty

        Node v l r ->
            if val <= v then
                Node v (insert val l) r
            else
                Node v l (insert val r)


toStr : Tree comparable -> String
toStr t =
    case t of
        Empty ->
            "()"

        Node v l r ->
            "[" ++ (toStr l) ++ "<-( " ++ (toString v) ++ " )->" ++ (toStr r) ++ "]"
