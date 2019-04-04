module Gradient exposing
    ( ColorStop
    , Gradient
    , gradientBackground
    , gradientString
    )

import Css as C
import Palette exposing (Color)


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
