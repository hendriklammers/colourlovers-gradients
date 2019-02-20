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
    , current : Index
    , error : Maybe String
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
            ( Model palettes 0 Nothing
            , Cmd.none
            )

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
viewGradient { stop1, stop2, stopsList } =
    let
        colorStop ( color, percentage ) =
            C.stop2
                (C.hex <| color)
                (C.pct <| percentage)

        gradient =
            C.linearGradient2
                (C.deg 90)
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


viewLoading : Html Msg
viewLoading =
    text "Loading color palettes"


viewError : Maybe String -> Html Msg
viewError error =
    case error of
        Just message ->
            text message

        Nothing ->
            text ""


viewContainer : List (Html Msg) -> Html Msg
viewContainer children =
    div
        [ css
            [ C.flex (C.int 1)
            , C.displayFlex
            ]
        ]
        (globalStyles :: children)


view : Model -> Html Msg
view model =
    viewContainer
        [ viewError model.error
        , getPalette model.current model.palettes
            |> Maybe.andThen paletteToGradient
            |> Maybe.map viewGradient
            |> Maybe.withDefault (text "Unable to show gradient")
        ]
