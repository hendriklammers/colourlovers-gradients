module Main exposing (main)

import Browser
import Html exposing (Html, div)
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
    { palettes : List Palette
    , gradient : Maybe Gradient
    }


type alias Color =
    String


type alias Palette =
    List Color


type alias Gradient =
    List ( Color, Float )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { palettes = []
      , gradient = Nothing
      }
    , getPalettes
    )


getPalettes : Cmd Msg
getPalettes =
    Http.get
        { url = "https://cors-anywhere.herokuapp.com/http://www.colourlovers.com/api/palettes/top?format=json"
        , expect = Http.expectJson ReceivePalettes palettesDecoder
        }


palettesDecoder : Decoder (List Palette)
palettesDecoder =
    Decode.list (Decode.field "colors" (Decode.list Decode.string))


type Msg
    = ReceivePalettes (Result Http.Error (List Palette))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivePalettes (Ok palettes) ->
            ( { model | palettes = Debug.log "palettes" palettes }, Cmd.none )

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
view { gradient } =
    div []
        []
