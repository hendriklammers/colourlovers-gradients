module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyUp)
import Html.Styled exposing (toUnstyled)
import Http
import Json.Decode as Decode exposing (Decoder)
import Model exposing (..)
import Ports exposing (confirmCopy)
import Process
import Random
import Settings exposing (settings)
import Task
import Time
import View exposing (view)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Init
    , getPalettes
    )


getPalettes : Cmd Msg
getPalettes =
    Http.get
        { url = settings.api
        , expect = Http.expectJson ReceiveData paletteListDecoder
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


selectGradient : Index -> List Palette -> Maybe Gradient
selectGradient index palettes =
    palettes
        |> List.drop index
        |> List.head
        |> Maybe.andThen paletteToGradient


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ReceiveData (Ok data), Init ) ->
            case selectGradient 0 data of
                Just gradient ->
                    ( Success
                        (Palettes data 0 1)
                        gradient
                        Nothing
                    , Cmd.none
                    )

                Nothing ->
                    ( Error "Unable to show gradient"
                    , Cmd.none
                    )

        ( ReceiveData (Err _), Init ) ->
            ( Error "Unable to load the color palettes from the server"
            , Cmd.none
            )

        ( Navigate nav, Success palettes { angle } notification ) ->
            let
                ( index, cmd ) =
                    navigate palettes.data palettes.active nav
            in
            case selectGradient index palettes.data of
                Just gradient ->
                    ( Success
                        { palettes | active = index }
                        { gradient | angle = angle }
                        notification
                    , cmd
                    )

                Nothing ->
                    ( Error "Unable to show gradient"
                    , Cmd.none
                    )

        ( Paginate nav, Success palettes gradient notification ) ->
            ( Success
                { palettes | page = paginate palettes nav }
                gradient
                notification
            , Cmd.none
            )

        ( Rotate angle, Success palettes gradient _ ) ->
            ( Success
                palettes
                { gradient | angle = gradient.angle + angle }
                Nothing
            , Cmd.none
            )

        ( CopyConfirmation ( success, value ), Success palettes gradient _ ) ->
            ( Success
                palettes
                gradient
                (Just
                    (case success of
                        True ->
                            "Copied CSS code to clipboard."

                        False ->
                            "Failed to copy CSS code to clipboard"
                    )
                )
            , delay 1500 CloseNotification
            )

        ( CloseNotification, Success palettes gradient _ ) ->
            ( Success palettes gradient Nothing
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


totalPages : List a -> Int
totalPages xs =
    ceiling <| toFloat (List.length xs) / toFloat settings.pageSize


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
                current

        Previous ->
            if current > 0 then
                current - 1

            else
                current

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
                    Navigate Previous

                "ArrowDown" ->
                    Navigate Next

                "Enter" ->
                    Navigate Random

                _ ->
                    NoOp
        )
        (Decode.field "key" Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyUp keyDecoder
        , confirmCopy CopyConfirmation
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
            Just (Gradient s1 s2 xs 180)

        _ ->
            Nothing


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view >> toUnstyled
        }
