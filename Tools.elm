module Tools exposing (..)


fac : Int -> Int
fac start =
    if start == 1 then
        1
    else if start == 0 then
        0
    else
        start * fac (start - 1)


fib : Int -> Int
fib start =
    if start == 1 || start == 0 then
        1
    else
        fib (start - 1) + fib (start - 2)


checkBraces : String -> Bool
checkBraces str =
    let
        cbIter : List Char -> Int -> Bool
        cbIter str braces =
            if braces < 0 then
                Debug.log "Dangling closing brace found" False
            else
                case str of
                    [] ->
                        Debug.log "Checking whether all braces are closed" (braces == 0)

                    head :: tail ->
                        if head == '(' then
                            cbIter tail (braces + 1)
                        else if head == ')' then
                            cbIter tail (braces - 1)
                        else
                            cbIter tail braces
    in
        cbIter (String.toList str) 0
