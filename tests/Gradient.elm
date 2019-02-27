module Gradient exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


widthPercentages : List Float -> List Float
widthPercentages widths =
    let
        total =
            List.sum (List.take (List.length widths - 1) widths)
    in
    widths
        |> List.map (\w -> w / total * 100)
        |> List.foldl
            (\val acc ->
                case acc of
                    x :: _ ->
                        x + val :: acc

                    _ ->
                        []
            )
            [ 0 ]
        |> List.drop 1
        |> List.reverse


suite : Test
suite =
    describe "Palette to gradient conversion"
        [ test "5 widths of same size" <|
            \_ ->
                let
                    input =
                        [ 0.2, 0.2, 0.2, 0.2, 0.2 ]

                    expected =
                        [ 0, 25, 50, 75, 100 ]
                in
                Expect.equal (widthPercentages input) expected
        , test "4 widths of same size" <|
            \_ ->
                let
                    input =
                        [ 0.25, 0.25, 0.25, 0.25 ]

                    expected =
                        [ 0, 33.33, 66.66, 100 ]
                in
                Expect.equal (widthPercentages input) expected
        ]
