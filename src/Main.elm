module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Http
import Json.Decode as Decode exposing (Decoder)
import ParseInt exposing (parseIntHex)
import Regex


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
    { red : Int
    , green : Int
    , blue : Int
    , alpha : Int
    }


type alias Palette =
    List Color


init : () -> ( Model, Cmd Msg )
init _ =
    ( [], getPalettes )


rrggbb : Regex.Regex
rrggbb =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^#?([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})?$"


hexToColor : String -> Result String Color
hexToColor hex =
    Regex.findAtMost 1 rrggbb hex
        |> List.head
        |> Maybe.map .submatches
        |> Maybe.map (List.filterMap identity)
        |> Result.fromMaybe "Unable to parse the string into a valid Color"
        |> Result.andThen
            (\color ->
                case List.map parseIntHex color of
                    [ Ok r, Ok g, Ok b, Ok a ] ->
                        Ok (Color r g b a)

                    [ Ok r, Ok g, Ok b ] ->
                        Ok (Color r g b 1)

                    _ ->
                        Err "Unable to parse the string into a valid Color"
            )


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
                Ok color ->
                    Decode.succeed color

                Err _ ->
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
