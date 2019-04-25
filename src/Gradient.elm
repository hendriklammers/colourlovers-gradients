module Gradient exposing
    ( ColorStop
    , Gradient
    , fromPalette
    , gradientBackground
    , gradientString
    , widthsToPercentages
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


aggregateWidths : List Float -> List Float
aggregateWidths xs =
    xs
        |> List.foldl
            (\cur acc ->
                Maybe.withDefault 0 (List.head acc) + cur * 100 :: acc
            )
            [ 0 ]
        |> List.reverse
        |> List.take (List.length xs)


equalWidths : List Float -> List Float
equalWidths xs =
    List.indexedMap
        (\i _ -> toFloat i * (100 / toFloat (List.length xs - 1)))
        xs


widthsToPercentages : List Float -> List Float
widthsToPercentages xs =
    case xs of
        x :: _ ->
            -- Check if all widths are equal
            if List.repeat (List.length xs) x == xs then
                equalWidths xs

            else
                aggregateWidths xs

        [] ->
            []


fromPalette : Palette -> Maybe ( Gradient, Palette )
fromPalette palette =
    let
        widths =
            widthsToPercentages palette.widths

        colorStops =
            List.map2 Tuple.pair palette.colors widths
    in
    case colorStops of
        s1 :: s2 :: xs ->
            Just ( Gradient s1 s2 xs 180, palette )

        _ ->
            Nothing
