module GradientTest exposing (suite)

import Expect
import Gradient
    exposing
        ( Gradient
        , fromPalette
        , gradientString
        , widthsToPercentages
        )
import Palette exposing (Palette)
import Test exposing (..)


palette : Palette
palette =
    { colors =
        [ "69D2E7"
        , "A7DBD8"
        , "E0E4CC"
        , "F38630"
        , "FA6900"
        ]
    , widths = [ 0.2, 0.2, 0.2, 0.2, 0.2 ]
    }


gradient : Gradient
gradient =
    { stop1 = ( "69D2E7", 0 )
    , stop2 = ( "A7DBD8", 25 )
    , stopsList =
        [ ( "E0E4CC", 50 )
        , ( "F38630", 75 )
        , ( "FA6900", 100 )
        ]
    , angle = 180
    }


suite : Test
suite =
    describe "Gradient module"
        [ widthsToPercentagesTest
        , paletteToGradientTest
        , gradientStringTest
        ]


gradientStringTest : Test
gradientStringTest =
    describe "Gradient to CSS string"
        [ test "gradientString function" <|
            \_ ->
                let
                    cssString =
                        "background-image: linear-gradient(180deg, #69D2E7 0%, #A7DBD8 25%, #E0E4CC 50%, #F38630 75%, #FA6900 100%);"
                in
                Expect.equal (gradientString gradient) cssString
        ]


paletteToGradientTest : Test
paletteToGradientTest =
    describe "Create Gradient from a Palette"
        [ test "Palette with 5 colors" <|
            \_ ->
                Expect.equal (fromPalette palette) (Just ( gradient, palette ))
        ]


widthsToPercentagesTest : Test
widthsToPercentagesTest =
    describe "Convert Palette widths to Gradient percentages"
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
