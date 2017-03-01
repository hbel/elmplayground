port module Main exposing (..)

import Sets as Set exposing (..)
import Tree exposing (..)
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
    describe "All Tests"
        [ describe "The tools functions"
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
        , describe "Set functions"
            [ describe "Singleton"
                [ test "{1} contains 1" <| \() -> Expect.true "1 is part of the set" (Set.contains 1 (Set.singleton 1))
                , test "{1} contains no 0" <| \() -> Expect.false "0 is not part of the set" (Set.contains 0 (Set.singleton 1))
                ]
            , describe "Union"
                [ test "Union of {1} and {2} contains 1" <| \() -> Expect.true "{1,2} contains 1" (Set.contains 1 (Set.union (Set.singleton 1) (Set.singleton 2)))
                , test "Union of {1} and {2} contains 2" <| \() -> Expect.true "{1,2} contains 2" (Set.contains 2 (Set.union (Set.singleton 1) (Set.singleton 2)))
                , test "Union of {1} and {2} contains no 0" <| \() -> Expect.false "{1,2} does not contain 0" (Set.contains 0 (Set.union (Set.singleton 1) (Set.singleton 2)))
                ]
            , describe "Intersection"
                [ test "Intersection of {1} and {2} contains no 1" <| \() -> Expect.false "{1,2} contains no 1" (Set.contains 1 (Set.intersection (Set.singleton 1) (Set.singleton 2)))
                , test "Intersection of {1} and {2} contains no 2" <| \() -> Expect.false "{1,2} contains no 2" (Set.contains 1 (Set.intersection (Set.singleton 1) (Set.singleton 2)))
                , test "Intersection of {1} and {2} contains no 0" <| \() -> Expect.false "{1,2} does not contain 0" (Set.contains 0 (Set.intersection (Set.singleton 1) (Set.singleton 2)))
                , test "Intersection of {1} and {1,2} contains 1" <|
                    \() ->
                        Expect.true "{1} contains 1"
                            (Set.contains 1
                                (Set.intersection (Set.singleton 1) (Set.union (Set.singleton 1) (Set.singleton 2)))
                            )
                ]
            , describe "Difference"
                [ test "Difference of {1} and {2} contains 1" <| \() -> Expect.true "{1}\\{2} contains 1" (Set.contains 1 (Set.difference (Set.singleton 1) (Set.singleton 2)))
                , test "Difference of {1} and {2} contains no 2" <| \() -> Expect.false "{1}\\{2} contains no 2" (Set.contains 2 (Set.difference (Set.singleton 1) (Set.singleton 2)))
                , test "Difference of {1} and {2} contains no 0" <| \() -> Expect.false "{1}\\{2} does not contain 0" (Set.contains 0 (Set.difference (Set.singleton 1) (Set.singleton 2)))
                ]
            ]
        , describe "Binary Trees"
            [ describe "Insertion"
                [ test "Empty tree" <| \() -> Expect.equal (Tree.toStr Tree.Empty) "()"
                , test "Tree with 1,2, and 3" <| \() -> Expect.equal (Tree.toStr (Tree.Empty |> Tree.insert 2 |> Tree.insert 1 |> Tree.insert 3)) "[[()<-( 1 )->()]<-( 2 )->[()<-( 3 )->()]]"
                ]
            ]
        ]
