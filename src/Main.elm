module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Http
import Json.Decode as Decode exposing (Decoder)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    List Palette


type alias Color =
    String


type alias Palette =
    List Color


init : () -> ( Model, Cmd Msg )
init _ =
    ( [], getPalettes )


hexToColor : String -> Maybe Color
hexToColor hex =
    Just hex


getPalettes : Cmd Msg
getPalettes =
    Http.get
        { url = "/data/palettes.json"
        , expect = Http.expectJson ReceivePalettes palettesDecoder
        }


colorDecoder : Decoder Color
colorDecoder =
    Decode.andThen
        (\hex ->
            case hexToColor hex of
                Just color ->
                    Decode.succeed color

                Nothing ->
                    Decode.fail
                        ("Unable to decode " ++ hex ++ " into a Color.")
        )
        Decode.string


palettesDecoder : Decoder (List Palette)
palettesDecoder =
    Decode.list (Decode.list colorDecoder)


type Msg
    = ReceivePalettes (Result Http.Error (List Palette))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivePalettes (Ok palettes) ->
            ( Debug.log "palettes" palettes, Cmd.none )

        ReceivePalettes (Err err) ->
            let
                log =
                    Debug.log "error" err
            in
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    text "beautiful gradients will come here"
