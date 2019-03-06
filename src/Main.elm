module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyUp)
import Css as C
import Css.Animations as A
import Css.Global exposing (body, global, html, selector)
import Html.Styled exposing (Html, div, li, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (attribute, css, id)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Ports exposing (confirmCopy)
import Random


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
    | ClipboardCopy ( Bool, String )
    | Ignore


type Navigation
    = Next
    | Previous
    | Jump Index
    | Random


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
        -- { url = "https://cors-anywhere.herokuapp.com/http://www.colourlovers.com/api/palettes/top?format=json&showPaletteWidths=1"
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
            case nav of
                Random ->
                    ( model
                    , Random.generate
                        (\index -> Navigate (Jump index))
                        (Random.int 0 (List.length model.palettes))
                    )

                _ ->
                    ( { model
                        | current =
                            navigateList model.palettes model.current nav
                      }
                    , Cmd.none
                    )

        Rotate angle ->
            ( { model | angle = model.angle + angle }, Cmd.none )

        ClipboardCopy ( success, value ) ->
            let
                log =
                    Debug.log "copied" value
            in
            ( model, Cmd.none )

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
            if index >= 0 && index < List.length xs then
                index

            else
                current

        _ ->
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

                "Enter" ->
                    Navigate Random

                _ ->
                    Ignore
        )
        (Decode.field "key" Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyUp keyDecoder
        , confirmCopy ClipboardCopy
        ]


globalStyles : Html Msg
globalStyles =
    global
        [ body
            [ C.margin (C.px 0)
            , C.height (C.vh 100)
            , C.displayFlex
            ]
        , html
            [ C.boxSizing C.borderBox ]
        , selector "*, *:before, *:after"
            [ C.boxSizing C.inherit ]
        ]


viewGradient : Float -> Maybe Gradient -> Html Msg
viewGradient angle gradient =
    div
        [ css
            [ C.displayFlex
            , C.flex <| C.int 1
            ]
        ]
        [ case gradient of
            Nothing ->
                text "Unable to show gradient"

            Just { stop1, stop2, stopsList } ->
                let
                    colorStop ( color, percentage ) =
                        C.stop2
                            (C.hex <| color)
                            (C.pct <| percentage)

                    background =
                        C.linearGradient2
                            (C.deg angle)
                            (colorStop stop1)
                            (colorStop stop2)
                            (List.map colorStop stopsList)

                    cssString =
                        "background-image: " ++ background.value ++ ";"
                in
                div
                    [ id "gradient"
                    , attribute "data-clipboard-text" cssString
                    , css
                        [ C.flex <| C.int 1
                        , C.backgroundImage background
                        ]
                    ]
                    []
        ]


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


viewPalette : Index -> Index -> Palette -> Html Msg
viewPalette current index { colors, widths } =
    let
        activeStyles =
            if current == index then
                C.batch
                    [ C.before
                        [ C.display C.block
                        , C.property "content" "''"
                        , C.position C.absolute
                        , C.left <| C.px 0
                        , C.top <| C.px 0
                        , C.width <| C.pct 100
                        , C.height <| C.pct 100
                        , C.zIndex <| C.int 2
                        , C.border3 (C.px 5) C.solid (C.hex "fff")
                        ]
                    ]

            else
                C.batch []
    in
    li
        [ css
            [ C.displayFlex
            , C.position C.relative
            , C.height <| C.px 150
            , C.cursor C.pointer
            , activeStyles
            ]
        , onClick (Navigate (Jump index))
        ]
        (List.map2 viewColor colors widths)


viewPalettes : Index -> List Palette -> Html Msg
viewPalettes current palettes =
    case palettes of
        [] ->
            text ""

        _ ->
            div
                [ css
                    [ C.flex3 (C.int 0) (C.int 0) (C.px 150)
                    , C.marginLeft C.auto
                    , C.backgroundColor <| C.hex "fff"
                    , C.overflowY C.scroll
                    ]
                ]
                [ ul
                    [ css
                        [ C.margin <| C.px 0
                        , C.padding <| C.px 0
                        , C.listStyle C.none
                        ]
                    ]
                    (List.indexedMap (viewPalette current) palettes)
                ]


viewPreloader : Html Msg
viewPreloader =
    let
        animation =
            A.keyframes
                [ ( 0, [ A.transform [ C.scaleY 1.0 ] ] )
                , ( 20, [ A.transform [ C.scaleY 0.6 ] ] )
                , ( 40, [ A.transform [ C.scaleY 1.0 ] ] )
                , ( 100, [ A.transform [ C.scaleY 1.0 ] ] )
                ]

        barStyle =
            C.batch
                [ C.width <| C.px 12
                , C.height <| C.px 60
                , C.animationName animation
                , C.animationDuration <| C.ms 1300
                , C.property "animation-iteration-count" "infinite"
                , C.property "animation-timing-function" "ease-in-out"
                ]
    in
    div
        [ css
            [ C.displayFlex
            , C.flex <| C.int 1
            , C.alignItems C.center
            , C.justifyContent C.center
            ]
        ]
        [ div
            [ css
                [ C.displayFlex ]
            ]
            (List.indexedMap
                (\i color ->
                    div
                        [ css
                            [ barStyle
                            , C.animationDelay <| C.ms (toFloat i * 100)
                            , C.backgroundColor <| C.hex color
                            ]
                        ]
                        []
                )
                [ "F8B195"
                , "F67280"
                , "C06C84"
                , "6C5B7B"
                , "355C7D"
                ]
            )
        ]


view : Model -> Html Msg
view { palettes, current, angle, error } =
    div
        [ css
            [ C.flex <| C.int 1
            , C.displayFlex
            ]
        ]
        [ globalStyles
        , viewPreloader

        -- , viewError error
        -- , getPalette current palettes
        --     |> Maybe.andThen paletteToGradient
        --     |> viewGradient angle
        -- , viewPalettes current palettes
        ]
