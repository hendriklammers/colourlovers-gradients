module Gradient exposing
    ( ColorStop
    , Gradient
    , fromPalette
    , gradientBackground
    , gradientString
    )

import Css as C
import Palette exposing (Color, Palette)


type alias ColorStop =
    ( Color, Float )


type alias Gradient =
    { stop1 : ColorStop
    , stop2 : ColorStop
    , stopsList : List ColorStop
    , angle : Float
    }


gradientString : Gradient -> String
gradientString gradient =
    let
        background =
            gradientBackground gradient
    in
    "background-image: " ++ background.value ++ ";"


gradientBackground : Gradient -> C.BackgroundImage (C.ListStyle {})
gradientBackground { stop1, stop2, stopsList, angle } =
    let
        colorStop ( color, percentage ) =
            C.stop2
                (C.hex <| color)
                (C.pct <| percentage)
    in
    C.linearGradient2
        (C.deg angle)
        (colorStop stop1)
        (colorStop stop2)
        (List.map colorStop stopsList)


fromPalette : Palette -> Maybe ( Gradient, Palette )
fromPalette palette =
    let
        colorStops =
            List.map2 Tuple.pair palette.colors (0 :: palette.widths)
                |> List.foldl
                    (\( color, width ) xs ->
                        ( color, widthToPercentage xs width ) :: xs
                    )
                    []
                |> List.reverse
    in
    case colorStops of
        s1 :: s2 :: xs ->
            Just ( Gradient s1 s2 xs 180, palette )

        _ ->
            Nothing


widthToPercentage : List ColorStop -> Float -> Float
widthToPercentage gradient width =
    case gradient of
        ( _, previousWidth ) :: _ ->
            previousWidth + width * 100

        _ ->
            0
