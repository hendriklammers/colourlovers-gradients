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
    List ( Color, Float )


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
            let
                current =
                    case nav of
                        Next ->
                            if model.current < List.length model.palettes - 1 then
                                model.current + 1

                            else
                                0

                        Previous ->
                            model.current - 1

                        Index index ->
                            index
            in
            ( { model | current = current }, Cmd.none )

        Ignore ->
            ( model, Cmd.none )


navigatePalettes : Int -> Navigation -> List a -> Int
navigatePalettes current nav palettes =
    case nav of
        Next ->
            if current < List.length palettes - 1 then
                current + 1

            else
                0

        Previous ->
            current - 1

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
viewGradient colors =
    case colors of
        ( c1, w1 ) :: ( c2, w2 ) :: xs ->
            let
                gradient =
                    C.linearGradient2
                        (C.deg 90)
                        (C.stop2 (C.hex c1) (C.pct w1))
                        (C.stop2 (C.hex c2) (C.pct w2))
                        (List.map (\( c, w ) -> C.stop2 (C.hex c) (C.pct w)) xs)
            in
            div
                [ css
                    [ C.flex (C.int 1)
                    , C.backgroundImage gradient
                    ]
                ]
                []

        _ ->
            text "Gradient consists of at least 2 colors"


colorStop : List ( Color, Float ) -> Float -> Float
colorStop gradient width =
    case gradient of
        ( _, previousWidth ) :: xs ->
            previousWidth + width * 100

        _ ->
            0


paletteToGradient : Palette -> Gradient
paletteToGradient { colors, widths } =
    List.map2 Tuple.pair colors (0 :: widths)
        |> List.foldl
            (\( color, width ) xs -> ( color, colorStop xs width ) :: xs)
            []
        |> List.reverse


getPalette : Int -> List Palette -> Maybe Palette
getPalette index palettes =
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
        , case getPalette model.current model.palettes of
            Just palette ->
                palette
                    |> paletteToGradient
                    |> viewGradient

            Nothing ->
                text "Gradient unavailable"
        ]
