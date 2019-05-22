module View exposing (view)

import Css as C
import Css.Animations as A
import Css.Global exposing (body, descendants, global, html, selector, typeSelector)
import Css.Media exposing (withMediaQuery)
import Css.Transitions as T
import Gradient exposing (Gradient, gradientBackground, gradientString)
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
        , ul
        )
import Html.Styled.Attributes exposing (attribute, css, id, title)
import Html.Styled.Events exposing (onClick)
import Model
    exposing
        ( Model(..)
        , Msg(..)
        , Navigation(..)
        , Notification(..)
        , Touch
        , View
        , totalPages
        )
import Palette exposing (Color, Palette, Palettes)
import Settings exposing (settings)
import Svg.Styled exposing (path, polygon, svg)
import Svg.Styled.Attributes as S


type alias Button =
    { icon : Html Msg
    , label : String
    , msg : Msg
    , size : Float
    , attributes : List (Attribute Msg)
    }


pxToRem : Float -> C.Rem
pxToRem px =
    C.rem (px / 16)


fontHelvetica : C.Style
fontHelvetica =
    C.batch
        [ C.fontFamilies
            [ "Helvetica Neue"
            , "Helvetica"
            , "Arial"
            , "sans-serif"
            ]
        ]


mediaMobile : List C.Style -> C.Style
mediaMobile styles =
    withMediaQuery
        [ "screen and (max-width: 480px)" ]
        styles


globalStyles : Html Msg
globalStyles =
    global
        [ body
            [ C.margin (C.px 0)
            , C.height (C.vh 100)
            , C.displayFlex
            , C.backgroundColor <| C.hex "C6C5C3"
            , C.fontWeight <| C.int 400
            , C.fontSize <| C.px 16
            , C.color <| C.hex "1C1614"
            , fontHelvetica
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
                , mediaMobile
                    [ C.width <| C.px 12
                    , C.height <| C.px 60
                    ]
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
                [ "AAFF00"
                , "FFAA00"
                , "FF00AA"
                , "AA00FF"
                , "00AAFF"
                ]
            )
        ]


viewButton : Touch -> Button -> Html Msg
viewButton touch { icon, label, msg, size, attributes } =
    let
        hoverStyles =
            if not touch then
                C.hover
                    [ descendants
                        [ typeSelector "div"
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
                    ]

            else
                C.batch []
    in
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
            , C.backgroundColor C.transparent
            , C.fontSize <| pxToRem 21
            , C.lineHeight <| pxToRem size
            , C.textAlign C.center
            , C.cursor C.pointer
            , hoverStyles
            ]
         , attribute "aria-label" label
         , title label
         ]
            ++ attributes
        )
        [ div
            [ css
                [ C.backgroundColor <| C.hex "C6C5C3"
                , C.boxShadow5
                    (C.px -2)
                    (C.px 2)
                    (C.px 2)
                    (C.px 0)
                    (C.rgba 0 0 0 0.7)
                , T.transition
                    [ T.transform3 100 0 T.easeOut
                    , T.boxShadow3 100 0 T.easeOut
                    ]
                ]
            ]
            [ icon
            ]
        ]


viewPaletteNavigation : Touch -> Palettes -> Html Msg
viewPaletteNavigation touch { data, page } =
    nav
        [ css
            [ C.displayFlex
            , C.justifyContent C.spaceBetween
            , C.padding <| C.px 6
            ]
        , attribute "aria-label" "Pagination"
        ]
        [ viewButton
            touch
            (Button
                (svg
                    [ S.width "16"
                    , S.height "14"
                    , S.viewBox "0 0 16 14"
                    , S.transform "scale(0.8, 0.8)"
                    ]
                    [ polygon
                        [ S.points "12 6 8.05025253 2.05025253 9.46446609 0.636038969 15.8284271 7 9.46446609 13.363961 8.05025253 11.9497475 12 8 0 8 0 6 11 6"
                        , S.transform "scale(-1, 1) translate(-16, 0)"
                        ]
                        []
                    ]
                )
                "Previous"
                (Paginate Previous)
                30
                []
            )
        , span
            [ css
                [ C.lineHeight <| pxToRem 30
                , C.textAlign C.center
                , C.fontSize <| pxToRem 18
                , mediaMobile [ C.display C.none ]
                ]
            ]
            [ span [] [ text <| String.fromInt (page + 1) ]
            , text "/"
            , span [] [ text <| String.fromInt (totalPages data) ]
            ]
        , viewButton
            touch
            (Button
                (svg
                    [ S.width "16"
                    , S.height "14"
                    , S.viewBox "0 0 16 14"
                    , S.transform "scale(0.8, 0.8)"
                    ]
                    [ polygon
                        [ S.points "12 6 8.05025253 2.05025253 9.46446609 0.636038969 15.8284271 7 9.46446609 13.363961 8.05025253 11.9497475 12 8 0 8 0 6 11 6" ]
                        []
                    ]
                )
                "Next"
                (Paginate Next)
                30
                []
            )
        ]


viewNavigation : View -> Html Msg
viewNavigation { gradient, touch } =
    let
        buttonSize =
            40

        buttons =
            [ Button
                (svg
                    [ S.width "14"
                    , S.height "18"
                    , S.viewBox "0 0 14 18"
                    ]
                    [ path
                        [ S.d "M7.12236839,4.00104805 C7.08166264,4.0003503 7.04087223,4 7,4 C3.13400675,4 0,7.13400675 0,11 C0,14.8659932 3.13400675,18 7,18 C10.8659932,18 14,14.8659932 14,11 L12,11 C12,13.7614237 9.76142375,16 7,16 C4.23857625,16 2,13.7614237 2,11 C2,8.23857625 4.23857625,6 7,6 C7.00692765,6 7.013852,6.00001409 7.02077304,6.00004224 L4.53553391,8.48528137 L5.94974747,9.89949494 L10.8994949,4.94974747 L5.94974747,0 L4.53553391,1.41421356 L7.12236839,4.00104805 L7.12236839,4.00104805 Z" ]
                        []
                    ]
                )
                "Rotate"
                (Rotate 45)
                buttonSize
                []
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
            , Button
                (svg
                    [ S.width "16"
                    , S.height "14"
                    , S.viewBox "0 0 16 14"
                    ]
                    [ polygon
                        [ S.points "12 6 8.05025253 2.05025253 9.46446609 0.636038969 15.8284271 7 9.46446609 13.363961 8.05025253 11.9497475 12 8 0 8 0 6 11 6"
                        , S.transform "scale(-1, 1) translate(-16, 0)"
                        ]
                        []
                    ]
                )
                "Previous"
                (Navigate Previous)
                buttonSize
                []
            , Button
                (svg
                    [ S.width "16"
                    , S.height "14"
                    , S.viewBox "0 0 16 14"
                    ]
                    [ polygon
                        [ S.points "12 6 8.05025253 2.05025253 9.46446609 0.636038969 15.8284271 7 9.46446609 13.363961 8.05025253 11.9497475 12 8 0 8 0 6 11 6" ]
                        []
                    ]
                )
                "Next"
                (Navigate Next)
                buttonSize
                []
            ]
    in
    nav
        [ css
            [ C.displayFlex
            , C.flexDirection C.row
            , C.justifyContent C.spaceBetween
            , C.position C.absolute
            , C.top <| C.px 0
            , C.right (C.px <| settings.paletteSize + 20)
            , C.margin2 (C.px 10) (C.px 10)
            , (List.length buttons * buttonSize)
                |> (+) ((List.length buttons - 1) * 8)
                |> toFloat
                |> C.px
                |> C.width
            , mediaMobile
                [ C.right (C.px <| (settings.paletteSize / 1.5))
                ]
            ]
        ]
        (List.map (viewButton touch) buttons)


viewPalettes : View -> Html Msg
viewPalettes { palettes, touch } =
    div
        [ css
            [ C.displayFlex
            , C.flex3 (C.int 0) (C.int 0) (C.px <| settings.paletteSize + 20)
            , C.flexDirection C.columnReverse
            , C.marginLeft C.auto
            , C.backgroundColor <| C.hex "fff"
            , mediaMobile
                [ C.flex3 (C.int 0) (C.int 0) (C.px <| (settings.paletteSize / 1.5))
                ]
            ]
        ]
        [ viewPaletteNavigation touch palettes
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
                    |> List.drop (palettes.page * settings.pageSize)
                    |> List.take settings.pageSize
                )
            ]
        ]


viewNotification : Notification -> Html Msg
viewNotification notification =
    case notification of
        Message message ->
            div
                [ css
                    [ C.position C.absolute
                    , C.left <| C.px 10
                    , C.top <| C.px 10
                    , C.padding2 (C.px 8) (pxToRem 10)
                    , C.backgroundColor <| C.hex "fff"
                    , C.boxShadow5
                        (C.px -3)
                        (C.px 3)
                        (C.px 2)
                        (C.px 0)
                        (C.rgba 0 0 0 0.7)
                    ]
                ]
                [ p
                    [ css
                        [ C.fontSize <| pxToRem 18
                        , C.margin <| C.px 0
                        ]
                    ]
                    [ text message ]
                ]

        Closed ->
            text ""


viewError : String -> Html Msg
viewError msg =
    div
        [ css
            [ flexCenterStyle ]
        ]
        [ div
            [ css
                [ C.maxWidth <| C.px 460
                , C.margin <| C.px 20
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


viewPalette : Int -> Int -> Palette -> Html Msg
viewPalette current index { colors, widths } =
    let
        activeStyles =
            if current == index then
                C.batch
                    [ C.before
                        [ C.display C.block
                        , C.property "content" "''"
                        , C.position C.absolute
                        , C.right <| C.pct -30
                        , C.top <| C.pct 25
                        , C.backgroundColor <| C.hex "C6C5C3"
                        , C.width <| C.pct 50
                        , C.height <| C.pct 50
                        , C.transform <| C.rotate (C.deg 45)
                        , C.boxShadow5
                            (C.px 2)
                            (C.px 3)
                            (C.px 2)
                            (C.px 0)
                            (C.rgba 0 0 0 0.7)
                        ]
                    ]

            else
                C.batch []
    in
    li
        [ onClick (Navigate (Jump index))
        ]
        [ div
            [ css
                [ C.displayFlex
                , C.position C.relative
                , C.height <| C.px settings.paletteSize
                , C.cursor C.pointer
                , C.overflow C.hidden
                , mediaMobile
                    [ C.height <| C.px (settings.paletteSize / 1.5) ]
                , activeStyles
                ]
            ]
            (List.map2 viewColor colors widths)
        ]


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
        Init _ ->
            viewContainer [ viewPreloader ]

        Error msg ->
            viewContainer [ viewError msg ]

        Success ({ gradient, notification } as viewModel) ->
            viewContainer
                [ viewNavigation viewModel
                , viewNotification notification
                , viewGradient gradient
                , viewPalettes viewModel
                ]
