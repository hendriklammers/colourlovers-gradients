module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyUp)
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
    , current : Int
    }


type alias Color =
    String


type alias Palette =
    { colors : List Color
    , widths : List Float
    }


type alias Gradient =
    Palette


type Msg
    = ReceivePalettes (Result Http.Error (List Palette))
    | Navigate Navigation
    | Ignore


type Navigation
    = Next
    | Previous
    | Index Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { palettes = []
      , current = 0
      }
    , getPalettes
    )


getPalettes : Cmd Msg
getPalettes =
    Http.get
        -- { url = "https://cors-anywhere.herokuapp.com/http://www.colourlovers.com/api/palettes/top?format=json"
        { url = "/data/palettes.json"
        , expect = Http.expectJson ReceivePalettes paletteListDecoder
        }


paletteDecoder : Decoder Palette
paletteDecoder =
    Decode.map2 Palette
        (Decode.field "colors" (Decode.list Decode.string))
        (Decode.field "colorWidths" (Decode.list Decode.float))


paletteListDecoder : Decoder (List Palette)
paletteListDecoder =
    Decode.list paletteDecoder


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivePalettes (Ok palettes) ->
            ( { model
                | palettes = palettes
              }
            , Cmd.none
            )

        ReceivePalettes (Err err) ->
            let
                log =
                    Debug.log "error" err
            in
            ( model, Cmd.none )

        Navigate nav ->
            ( { model | current = navigatePalettes model nav }
            , Cmd.none
            )

        Ignore ->
            ( model, Cmd.none )


navigatePalettes : Model -> Navigation -> Int
navigatePalettes { current, palettes } nav =
    case nav of
        Next ->
            if current < List.length palettes - 1 then
                current + 1

            else
                0

        Previous ->
            if current > 0 then
                current - 1

            else
                List.length palettes - 1

        Index index ->
            index


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map
        (\key ->
            case key of
                "ArrowRight" ->
                    Navigate Next

                "ArrowLeft" ->
                    Navigate Previous

                _ ->
                    Ignore
        )
        (Decode.field "key" Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyUp keyDecoder


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
viewGradient { colors, widths } =
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


getGradient : Int -> List Palette -> Maybe Gradient
getGradient index palettes =
    palettes
        |> List.drop index
        |> List.head


view : Model -> Html Msg
view model =
    div
        [ css
            [ C.flex (C.int 1)
            , C.displayFlex
            ]
        ]
        [ globalStyles
        , case getGradient model.current model.palettes of
            Just gradient ->
                viewGradient gradient

            Nothing ->
                text "Gradient unavailable"
        ]
