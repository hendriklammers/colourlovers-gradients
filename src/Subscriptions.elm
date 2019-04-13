module Subscriptions exposing (subscriptions)

import Browser.Events exposing (onKeyUp)
import Json.Decode as Decode
import Model exposing (Model, Msg(..), Navigation(..))
import Ports exposing (confirmCopy)


subscriptions : Model -> Sub Msg
subscriptions _ =
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
                    Navigate Next

                "ArrowLeft" ->
                    Navigate Previous

                "ArrowUp" ->
                    Navigate Previous

                "ArrowDown" ->
                    Navigate Next

                "Enter" ->
                    Navigate Random

                _ ->
                    NoOp
        )
        (Decode.field "key" Decode.string)
