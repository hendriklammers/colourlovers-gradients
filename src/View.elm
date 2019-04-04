module View exposing (view)

import Css as C
import Css.Animations as A
import Css.Global exposing (body, global, html, selector)
import Html.Styled
    exposing
        ( Attribute
        , Html
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
import Html.Styled.Attributes exposing (attribute, css, id, title)
import Html.Styled.Events exposing (onClick)
import Model
    exposing
        ( Gradient
        , Model(..)
        , Notification
        )
import Palette exposing (Color, Index, Palette, Palettes)
import Settings exposing (settings)
import Svg.Styled exposing (path, svg)
import Svg.Styled.Attributes as S
import Update exposing (Msg(..), Navigation(..))


type alias Button =
    { icon : Html Msg
    , label : String
    , msg : Msg
    , size : Float
    , attributes : List (Attribute Msg)
    }


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
        , selector "body > div"
            [ C.flex <| C.int 1
            , C.displayFlex
            ]
        , selector "*, *:before, *:after"
            [ C.boxSizing C.inherit ]
        ]


viewContainer : List (Html Msg) -> Html Msg
viewContainer content =
    div
        [ css
            [ C.flex <| C.int 1
            , C.displayFlex
            , C.position C.relative
            ]
        ]
        (globalStyles :: content)


pxToRem : Float -> C.Rem
pxToRem px =
    C.rem (px / 16)


flexCenterStyle : C.Style
flexCenterStyle =
    C.batch
        [ C.displayFlex
        , C.flex <| C.int 1
        , C.alignItems C.center
        , C.justifyContent C.center
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


viewButton : Button -> Html Msg
viewButton { icon, label, msg, size, attributes } =
    button
        ([ onClick msg
         , css
            [ C.display C.block
            , C.padding <| C.px 0
            , C.width <| C.px size
            , C.height <| C.px size
            , C.overflow C.visible
            , C.border <| C.px 0
            , C.borderRadius <| C.px 0
            , C.fontSize <| pxToRem 21
            , C.lineHeight <| pxToRem size
            , C.textAlign C.center
            , C.cursor C.pointer
            , C.backgroundColor <| C.hex "C6C5C3"
            , C.boxShadow5
                (C.px -2)
                (C.px 2)
                (C.px 2)
                (C.px 0)
                (C.rgba 0 0 0 0.7)
            , C.hover
                [ C.backgroundColor <| C.hex "A4FF44"
                , C.transform <| C.translate2 (C.px -1) (C.px 2)
                , C.boxShadow5
                    (C.px 0)
                    (C.px 0)
                    (C.px 0)
                    (C.px 0)
                    (C.rgba 0 0 0 0.7)
                ]
            ]
         , attribute "aria-label" label
         , title label
         ]
            ++ attributes
        )
        [ icon ]


totalPages : List a -> Int
totalPages xs =
    ceiling <| toFloat (List.length xs) / toFloat settings.pageSize


viewPaletteNavigation : Palettes -> Html Msg
viewPaletteNavigation { data, page } =
    nav
        [ css
            [ C.displayFlex
            , C.justifyContent C.spaceBetween
            , C.padding <| C.px 6
            ]
        , attribute "aria-label" "Pagination"
        ]
        [ viewButton (Button (text "←") "Previous" (Paginate Previous) 30 [])
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
        , viewButton (Button (text "→") "Next" (Paginate Next) 30 [])
        ]


viewNavigation : Gradient -> Html Msg
viewNavigation gradient =
    let
        buttonSize =
            40

        buttons =
            [ Button (text "←") "Previous" (Navigate Previous) buttonSize []
            , Button (text "→") "Next" (Navigate Next) buttonSize []
            , Button (text "↻") "Rotate" (Rotate 45) buttonSize []
            , Button
                (svg
                    [ S.width "18"
                    , S.height "18"
                    , S.viewBox "0 0 18 18"
                    ]
                    [ path
                        [ S.d "M6,6 L6,16 L16,16 L16,6 L6,6 Z M0,0 L14,0 L14,2 L2,2 L2,14 L0,14 L0,0 Z M4,4 L18,4 L18,18 L4,18 L4,4 Z" ]
                        []
                    ]
                )
                "Copy CSS code"
                NoOp
                buttonSize
                [ id "clipboard-copy"
                , attribute "data-clipboard-text" (gradientString gradient)
                ]
            ]
    in
    nav
        [ css
            [ C.displayFlex
            , C.flexDirection C.row
            , C.justifyContent C.spaceBetween
            , C.position C.absolute
            , C.top <| C.px 0
            , C.left <| C.px 0
            , C.margin2 (C.px 10) (C.px 10)
            , (List.length buttons * buttonSize)
                |> (+) ((List.length buttons - 1) * 8)
                |> toFloat
                |> C.px
                |> C.width
            ]
        ]
        (List.map viewButton buttons)


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


viewNotification : Notification -> Html Msg
viewNotification message =
    div
        [ css
            [ C.position C.absolute
            , C.left <| C.px 10
            , C.top <| C.px 60
            , C.padding2 (C.px 8) (pxToRem 10)
            , C.backgroundColor <| C.hex "fff"
            , C.boxShadow5
                (C.px -3)
                (C.px 3)
                (C.px 2)
                (C.px 1)
                (C.rgba 0 0 0 0.7)
            ]
        ]
        [ p
            [ css
                [ C.fontSize <| pxToRem 21
                , C.margin <| C.px 0
                ]
            ]
            [ text message ]
        ]


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
                , C.boxShadow5
                    (C.px -3)
                    (C.px 3)
                    (C.px 2)
                    (C.px 1)
                    (C.rgba 0 0 0 0.7)
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


gradientString : Gradient -> String
gradientString gradient =
    let
        background =
            gradientBackground gradient
    in
    "background-image: " ++ background.value ++ ";"


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
    div
        [ css
            [ C.displayFlex
            , C.flex <| C.int 1
            ]
        ]
        [ div
            [ css
                [ C.flex <| C.int 1
                , C.backgroundImage <| gradientBackground gradient
                ]
            ]
            []
        ]


view : Model -> Html Msg
view model =
    case model of
        Init ->
            viewContainer [ viewPreloader ]

        Error msg ->
            viewContainer [ viewError msg ]

        Success palettes gradient notification ->
            viewContainer
                [ viewNavigation gradient
                , case notification of
                    Just message ->
                        viewNotification message

                    Nothing ->
                        text ""
                , viewGradient gradient
                , viewPalettes palettes
                ]