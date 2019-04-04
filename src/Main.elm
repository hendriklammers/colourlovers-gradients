module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyUp)
import Html.Styled exposing (toUnstyled)
import Http
import Json.Decode as Decode exposing (Decoder)
import Palette exposing (paletteListDecoder)
import Ports exposing (confirmCopy)
import Settings exposing (settings)
import Update exposing (Model(..), Msg(..), update)
import View exposing (view)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Init
    , getPalettes
    )


getPalettes : Cmd Msg
getPalettes =
    Http.get
        { url = settings.api
        , expect = Http.expectJson ReceiveData paletteListDecoder
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyUp keyDecoder
        , confirmCopy CopyConfirmation
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map
        (\key ->
            case key of
                "ArrowRight" ->
                    Navigate Palette.Next

                "ArrowLeft" ->
                    Navigate Palette.Previous

                "ArrowUp" ->
                    Navigate Palette.Previous

                "ArrowDown" ->
                    Navigate Palette.Next

                "Enter" ->
                    Navigate Palette.Random

                _ ->
                    NoOp
        )
        (Decode.field "key" Decode.string)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view >> toUnstyled
        }
