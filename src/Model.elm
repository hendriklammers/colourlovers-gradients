module Model exposing
    ( ColorStop
    , Gradient
    , Model(..)
    , Notification
    )

import Palette exposing (Color, Palettes)


type Model
    = Init
    | Error String
    | Success Palettes Gradient (Maybe Notification)


type alias Notification =
    String


type alias ColorStop =
    ( Color, Float )


type alias Gradient =
    { stop1 : ColorStop
    , stop2 : ColorStop
    , stopsList : List ColorStop
    , angle : Float
    }
