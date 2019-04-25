module Example exposing (suite)

import Expect exposing (Expectation)
import Gradient exposing (widthsToPercentages)
import Test exposing (..)


suite : Test
suite =
    describe "Gradient from Palette"
        [ test "5 equal widths" <|
            \_ ->
                let
                    widths =
                        [ 0.2, 0.2, 0.2, 0.2, 0.2 ]

                    percentages =
                        [ 0, 25, 50, 75, 100 ]
                in
                Expect.equal percentages (widthsToPercentages widths)
        , test "2 equal widths" <|
            \_ ->
                let
                    widths =
                        [ 0.5, 0.5 ]

                    percentages =
                        [ 0, 100 ]
                in
                Expect.equal percentages (widthsToPercentages widths)
        , test "different widths" <|
            \_ ->
                let
                    widths =
                        [ 0.1, 0.3, 0.2, 0.3, 0.1 ]

                    percentages =
                        [ 0, 10, 40, 60, 90 ]
                in
                Expect.equal percentages (widthsToPercentages widths)
        ]
