module Main exposing (main)

import Browser
import Html.Styled exposing (toUnstyled)
import Http
import Model exposing (Flags, Model(..), Msg(..), Navigation(..), update)
import Palette exposing (paletteListDecoder)
import Settings exposing (settings)
import Subscriptions exposing (subscriptions)
import View exposing (view)


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Init flags
    , Http.get
        { url = settings.api
        , expect = Http.expectJson ReceiveData paletteListDecoder
        }
    )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view >> toUnstyled
        }
