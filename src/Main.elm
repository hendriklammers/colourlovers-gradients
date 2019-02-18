module Main exposing (main)

import Browser
import Css as C
import Css.Global exposing (body, global)
import Html.Styled exposing (Html, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Http
import Json.Decode as Decode exposing (Decoder)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view >> toUnstyled
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
    Palette


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
        -- { url = "https://cors-anywhere.herokuapp.com/http://www.colourlovers.com/api/palettes/top?format=json"
        { url = "/data/palettes.json"
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
            ( { model
                | palettes = palettes
                , gradient = List.head palettes
              }
            , Cmd.none
            )

        ReceivePalettes (Err err) ->
            let
                log =
                    Debug.log "error" err
            in
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


globalStyles : Html Msg
globalStyles =
    global
        [ body
            [ C.margin (C.px 0)
            , C.height (C.vh 100)
            , C.displayFlex
            ]
        ]


viewGradient : Gradient -> Html Msg
viewGradient colors =
    case colors of
        color1 :: color2 :: rest ->
            let
                gradient =
                    C.linearGradient2
                        (C.deg 90)
                        (C.stop (C.hex color1))
                        (C.stop (C.hex color2))
                        (List.map (\c -> C.stop (C.hex c)) rest)
            in
            div
                [ css
                    [ C.flex (C.int 1)
                    , C.backgroundImage gradient
                    ]
                ]
                []

        _ ->
            text ""


view : Model -> Html Msg
view model =
    div
        [ css
            [ C.flex (C.int 1)
            , C.displayFlex
            ]
        ]
        [ globalStyles
        , case model.gradient of
            Just gradient ->
                viewGradient gradient

            Nothing ->
                text "No gradient available"
        ]
