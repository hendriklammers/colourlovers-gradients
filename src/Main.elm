module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyUp)
import Css as C
import Css.Global exposing (body, global)
import Html.Styled exposing (Html, div, li, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
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
    , current : Index
    , error : Maybe String
    , angle : Float
    }


type alias Color =
    String


type alias Index =
    Int


type alias Palette =
    { colors : List Color
    , widths : List Float
    }


type alias ColorStop =
    ( Color, Float )


type alias Gradient =
    { stop1 : ColorStop
    , stop2 : ColorStop
    , stopsList : List ColorStop
    }


type Msg
    = ReceivePalettes (Result Http.Error (List Palette))
    | Navigate Navigation
    | Rotate Float
    | Ignore


type Navigation
    = Next
    | Previous
    | Jump Index


init : () -> ( Model, Cmd Msg )
init _ =
    ( { palettes = []
      , current = 0
      , error = Nothing
      , angle = 90
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
            ( { model | palettes = palettes }, Cmd.none )

        ReceivePalettes (Err _) ->
            ( { model
                | error =
                    Just "Unable to load the color palettes from the server"
              }
            , Cmd.none
            )

        Navigate nav ->
            ( { model
                | current =
                    navigateList model.palettes model.current nav
              }
            , Cmd.none
            )

        Rotate angle ->
            ( { model | angle = model.angle + angle }, Cmd.none )

        Ignore ->
            ( model, Cmd.none )


navigateList : List a -> Index -> Navigation -> Index
navigateList xs current nav =
    case nav of
        Next ->
            if current < List.length xs - 1 then
                current + 1

            else
                0

        Previous ->
            if current > 0 then
                current - 1

            else
                List.length xs - 1

        Jump index ->
            if index >= 0 && index < List.length xs - 1 then
                index

            else
                current


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
                    Rotate -90

                "ArrowDown" ->
                    Rotate 90

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


viewGradient : Float -> Gradient -> Html Msg
viewGradient angle { stop1, stop2, stopsList } =
    let
        colorStop ( color, percentage ) =
            C.stop2
                (C.hex <| color)
                (C.pct <| percentage)

        gradient =
            C.linearGradient2
                (C.deg angle)
                (colorStop stop1)
                (colorStop stop2)
                (List.map colorStop stopsList)
    in
    div
        [ css
            [ C.flex (C.int 1)
            , C.backgroundImage gradient
            ]
        ]
        []


widthToPercentage : List ColorStop -> Float -> Float
widthToPercentage gradient width =
    case gradient of
        ( _, previousWidth ) :: xs ->
            previousWidth + width * 100

        _ ->
            0


paletteToGradient : Palette -> Maybe Gradient
paletteToGradient { colors, widths } =
    let
        colorStops =
            List.map2 Tuple.pair colors (0 :: widths)
                |> List.foldl
                    (\( color, width ) xs ->
                        ( color, widthToPercentage xs width ) :: xs
                    )
                    []
                |> List.reverse
    in
    case colorStops of
        s1 :: s2 :: xs ->
            Just (Gradient s1 s2 xs)

        _ ->
            Nothing


getPalette : Index -> List Palette -> Maybe Palette
getPalette index palettes =
    palettes
        |> List.drop index
        |> List.head


viewError : Maybe String -> Html Msg
viewError error =
    case error of
        Just message ->
            text message

        Nothing ->
            text ""


viewColor : Color -> Float -> Html Msg
viewColor color width =
    div
        [ css
            [ C.flex3 (C.int 0) (C.int 0) (C.pct (width * 100))
            , C.backgroundColor (C.hex color)
            ]
        ]
        []


viewPalette : Index -> Palette -> Html Msg
viewPalette index { colors, widths } =
    li
        [ css
            [ C.displayFlex
            , C.height (C.px 150)
            , C.cursor C.pointer
            ]
        , onClick (Navigate (Jump index))
        ]
        (List.map2 viewColor colors widths)


viewPalettes : List Palette -> Html Msg
viewPalettes palettes =
    div
        [ css
            [ C.flex3 (C.int 0) (C.int 0) (C.px 150)
            , C.backgroundColor (C.hex "000")
            , C.overflowY C.scroll
            ]
        ]
        [ ul
            [ css
                [ C.margin (C.px 0)
                , C.padding (C.px 0)
                , C.listStyle C.none
                ]
            ]
            (List.indexedMap viewPalette palettes)
        ]


view : Model -> Html Msg
view { palettes, current, angle, error } =
    div
        [ css
            [ C.flex (C.int 1)
            , C.displayFlex
            ]
        ]
        [ globalStyles
        , viewError error
        , getPalette current palettes
            |> Maybe.andThen paletteToGradient
            |> Maybe.map (viewGradient angle)
            |> Maybe.withDefault (text "Unable to show gradient")
        , viewPalettes palettes
        ]
