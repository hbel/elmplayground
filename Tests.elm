port module Main exposing (..)

import Tools exposing (fib, fac, checkBraces)
import Test exposing (Test, test, fuzzWith, describe)
import Expect
import Fuzz exposing (intRange)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit suite


port emit : ( String, Value ) -> Cmd msg


suite : Test
suite =
    describe "The tools functions"
        [ describe "Fibonacci numbers"
            [ test "Is 1 for 0" <| \() -> Expect.equal (fib 0) 1
            , test "Is 1 for 1" <| \() -> Expect.equal (fib 1) 1
            , fuzzWith { runs = 50 } (intRange 2 20) "Is the sum of fib(i-1) and fib(i-2)" <| \x -> Expect.equal (fib x) (fib (x - 1) + fib (x - 2))
            ]
        , describe "Faculty"
            [ test "Is 0 for 0" <| \() -> Expect.equal (fac 0) 0
            , test "Is 1 for 1" <| \() -> Expect.equal (fac 1) 1
            , test "Is 120 for 5" <| \() -> Expect.equal (fac 5) 120
            ]
        , describe "Brace checker"
            [ test "Is True for (())" <| \() -> Expect.equal (checkBraces "(())") True
            , test "Is True for Hello" <| \() -> Expect.equal (checkBraces "Hello") True
            , test "Is False for (()" <| \() -> Expect.equal (checkBraces "(()") False
            ]
        ]
