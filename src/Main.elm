module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyUp)
import Css as C
import Css.Animations as A
import Css.Global exposing (body, global, html, selector)
import Html.Styled
    exposing
        ( Html
        , button
        , div
        , h3
        , li
        , nav
        , p
        , span
        , text
        , toUnstyled
        , ul
        )
import Html.Styled.Attributes exposing (attribute, css, id)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Ports exposing (confirmCopy)
import Process
import Random
import Task
import Time


type Model
    = Init
    | Error String
    | Success Palettes Gradient


type alias Palettes =
    { data : List Palette
    , active : Index
    , page : Index
    }


type alias Palette =
    { colors : List Color
    , widths : List Float
    }


type alias Color =
    String


type alias Index =
    Int


type alias ColorStop =
    ( Color, Float )


type alias Gradient =
    { stop1 : ColorStop
    , stop2 : ColorStop
    , stopsList : List ColorStop
    , angle : Float
    }


type Msg
    = ReceiveData (Result Http.Error (List Palette))
    | Navigate Navigation
    | Paginate Navigation
    | Rotate Float
    | ClipboardCopy ( Bool, String )
    | Ignore
    | Delay Float Msg


type Navigation
    = Next
    | Previous
    | Jump Index
    | Random


type alias Settings =
    { pageSize : Int
    , paletteSize : Float
    }


settings : Settings
settings =
    { pageSize = 50
    , paletteSize = 120
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Init
    , getPalettes
    )


getPalettes : Cmd Msg
getPalettes =
    Http.get
        { url = "/data/palettes.json"
        , expect =
            Http.expectJson
                (ReceiveData >> Delay 1000)
                paletteListDecoder
        }


paletteDecoder : Decoder Palette
paletteDecoder =
    Decode.map2 Palette
        (Decode.field "colors" (Decode.list Decode.string))
        (Decode.field "colorWidths" (Decode.list Decode.float))


paletteListDecoder : Decoder (List Palette)
paletteListDecoder =
    Decode.list paletteDecoder


delay : Float -> Msg -> Cmd Msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


selectGradient : Index -> Palettes -> Model
selectGradient index palettes =
    case
        palettes.data
            |> List.drop index
            |> List.head
            |> Maybe.andThen paletteToGradient
    of
        Just gradient ->
            Success
                { palettes | active = index }
                gradient

        Nothing ->
            Error "Unable to show gradient"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ReceiveData (Ok data), Init ) ->
            ( selectGradient 0 (Palettes data 0 1), Cmd.none )

        ( ReceiveData (Err err), Init ) ->
            let
                debug =
                    Debug.log "Error" err
            in
            ( Error "Unable to load the color palettes from the server"
            , Cmd.none
            )

        ( Navigate nav, Success palettes gradient ) ->
            let
                ( index, cmd ) =
                    navigate palettes.data palettes.active nav
            in
            ( selectGradient index palettes, cmd )

        ( Paginate nav, Success palettes gradient ) ->
            ( Success { palettes | page = paginate palettes nav } gradient
            , Cmd.none
            )

        ( Rotate angle, Success palettes gradient ) ->
            ( Success palettes { gradient | angle = gradient.angle + angle }
            , Cmd.none
            )

        ( ClipboardCopy ( success, value ), Success palettes gradient ) ->
            let
                log =
                    Debug.log "copied" value
            in
            ( model, Cmd.none )

        ( Delay time message, _ ) ->
            ( model, delay time message )

        _ ->
            ( model, Cmd.none )


paginate : Palettes -> Navigation -> Index
paginate { data, page } nav =
    case nav of
        Next ->
            if page < totalPages data then
                page + 1

            else
                page

        Previous ->
            if page > 1 then
                page - 1

            else
                page

        _ ->
            page


navigate : List a -> Index -> Navigation -> ( Index, Cmd Msg )
navigate xs current nav =
    ( case nav of
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

        Jump i ->
            if i >= 0 && i < List.length xs then
                i

            else
                current

        Random ->
            current
    , case nav of
        Random ->
            Random.generate
                (\i -> Navigate (Jump i))
                (Random.int 0 (List.length xs))

        _ ->
            Cmd.none
    )


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


flexCenterStyle : C.Style
flexCenterStyle =
    C.batch
        [ C.displayFlex
        , C.flex <| C.int 1
        , C.alignItems C.center
        , C.justifyContent C.center
        ]


pxToRem : Int -> C.Rem
pxToRem px =
    C.rem (toFloat px / 16)


globalStyles : Html Msg
globalStyles =
    global
        [ body
            [ C.margin (C.px 0)
            , C.height (C.vh 100)
            , C.displayFlex
            , C.fontFamilies
                [ "TimesNewRoman"
                , "Times New Roman"
                , "Times"
                , "serif"
                ]
            , C.fontWeight <| C.int 300
            , C.fontSize <| C.px 16
            , C.color <| C.hex "1C1614"
            ]
        , html
            [ C.boxSizing C.borderBox ]
        , selector "*, *:before, *:after"
            [ C.boxSizing C.inherit ]
        ]


gradientBackground : Gradient -> C.BackgroundImage (C.ListStyle {})
gradientBackground { stop1, stop2, stopsList, angle } =
    let
        colorStop ( color, percentage ) =
            C.stop2
                (C.hex <| color)
                (C.pct <| percentage)
    in
    C.linearGradient2
        (C.deg angle)
        (colorStop stop1)
        (colorStop stop2)
        (List.map colorStop stopsList)


viewGradient : Gradient -> Html Msg
viewGradient gradient =
    let
        background =
            gradientBackground gradient

        cssString =
            "background-image: " ++ background.value ++ ";"
    in
    div
        [ css
            [ C.displayFlex
            , C.flex <| C.int 1
            ]
        ]
        [ div
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
            Just (Gradient s1 s2 xs 90)

        _ ->
            Nothing


viewError : String -> Html Msg
viewError msg =
    div
        [ css
            [ flexCenterStyle
            , C.backgroundImage <|
                gradientBackground
                    (Gradient
                        ( "1C1614", 0 )
                        ( "E9E8E8", 70 )
                        [ ( "FFF", 90 ) ]
                        90
                    )
            ]
        ]
        [ div
            [ css
                [ C.width <| C.px 460
                , C.padding2 (C.em 1) (C.em 1.5)
                , C.backgroundColor <| C.hex "E32545"
                , C.boxShadow5 (C.px -3) (C.px 3) (C.px 2) (C.px 1) (C.hex "333")
                , C.color <| C.hex "FFF"
                ]
            ]
            [ h3
                [ css
                    [ C.margin <| C.px 0
                    , C.fontSize <| pxToRem 30
                    , C.fontWeight <| C.int 400
                    ]
                ]
                [ text "An error occurred!" ]
            , p [ css [ C.fontSize <| pxToRem 18 ] ]
                [ text msg ]
            ]
        ]


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
            , C.height <| C.px settings.paletteSize
            , C.cursor C.pointer
            , activeStyles
            ]
        , onClick (Navigate (Jump index))
        ]
        (List.map2 viewColor colors widths)


totalPages : List a -> Int
totalPages xs =
    ceiling <| toFloat (List.length xs) / toFloat settings.pageSize


viewPaletteNavigation : Palettes -> Html Msg
viewPaletteNavigation { data, page } =
    let
        buttonStyle =
            C.batch
                [ C.display C.block
                , C.padding <| C.px 0
                , C.width <| C.px 30
                , C.height <| C.px 30
                , C.overflow C.visible
                , C.border <| C.px 0
                , C.borderRadius <| C.px 0
                , C.fontSize <| pxToRem 21
                , C.lineHeight <| pxToRem 30
                , C.textAlign C.center
                , C.cursor C.pointer
                , C.backgroundColor <| C.hex "C6C5C3"
                , C.boxShadow5 (C.px -2) (C.px 2) (C.px 2) (C.px 0) (C.hex "333")
                , C.hover
                    [ C.backgroundColor <| C.hex "A4FF44"
                    ]
                ]
    in
    nav
        [ css
            [ C.displayFlex
            , C.justifyContent C.spaceBetween
            , C.padding <| C.px 6
            ]
        , attribute "aria-label" "Pagination"
        ]
        [ button
            [ onClick (Paginate Previous)
            , css [ buttonStyle ]
            , attribute "aria-label" "Previous"
            ]
            [ text "←" ]
        , span
            [ css
                [ C.lineHeight <| pxToRem 30
                , C.textAlign <| C.center
                , C.fontSize <| pxToRem 21
                ]
            ]
            [ text <|
                String.fromInt page
                    ++ "/"
                    ++ String.fromInt (totalPages data)
            ]
        , button
            [ onClick (Paginate Next)
            , css [ buttonStyle ]
            , attribute "aria-label" "Next"
            ]
            [ text "→" ]
        ]


viewPalettes : Palettes -> Html Msg
viewPalettes palettes =
    div
        [ css
            [ C.displayFlex
            , C.flex3 (C.int 0) (C.int 0) (C.px <| settings.paletteSize + 20)
            , C.flexDirection C.columnReverse
            , C.marginLeft C.auto
            , C.backgroundColor <| C.hex "fff"
            ]
        ]
        [ viewPaletteNavigation palettes
        , div
            [ css
                [ C.overflowY C.scroll
                , C.flex <| C.int 1
                ]
            ]
            [ ul
                [ css
                    [ C.margin <| C.px 0
                    , C.padding <| C.px 0
                    , C.listStyle C.none
                    ]
                ]
                (List.indexedMap
                    (viewPalette palettes.active)
                    palettes.data
                    |> List.drop ((palettes.page - 1) * settings.pageSize)
                    |> List.take settings.pageSize
                )
            ]
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
                [ C.width <| C.px 16
                , C.height <| C.px 80
                , C.animationName animation
                , C.animationDuration <| C.ms 1300
                , C.property "animation-iteration-count" "infinite"
                , C.property "animation-timing-function" "ease-in-out"
                ]
    in
    div
        [ css [ flexCenterStyle ] ]
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
                [ "1C1614"
                , "55514F"
                , "8E8B8A"
                , "E9E8E8"
                , "C6C5C3"
                ]
            )
        ]


viewContainer : List (Html Msg) -> Html Msg
viewContainer content =
    div
        [ css
            [ C.flex <| C.int 1
            , C.displayFlex
            ]
        ]
        (globalStyles :: content)


view : Model -> Html Msg
view model =
    case model of
        Init ->
            viewContainer [ viewPreloader ]

        Error msg ->
            viewContainer [ viewError msg ]

        Success palettes gradient ->
            viewContainer
                [ viewGradient gradient
                , viewPalettes palettes
                ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view >> toUnstyled
        }
