module Update exposing
    ( Model(..)
    , Msg(..)
    , Navigation(..)
    , Notification
    , totalPages
    , update
    )

import Gradient exposing (ColorStop, Gradient)
import Http
import Palette exposing (Color, Palette, Palettes)
import Ports exposing (updateFavicon)
import Process
import Random
import Settings exposing (settings)
import Task
import Time


type Model
    = Init
    | Error String
    | Success Palettes Gradient (Maybe Notification)


type alias Notification =
    String


type Msg
    = ReceiveData (Result Http.Error (List Palette))
    | Navigate Navigation
    | Paginate Navigation
    | Rotate Float
    | CopyConfirmation ( Bool, String )
    | CloseNotification
    | NoOp


type Navigation
    = Next
    | Previous
    | Jump Int
    | Random


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ReceiveData (Ok data), Init ) ->
            case selectGradient 0 data of
                Just ( gradient, palette ) ->
                    ( Success
                        (Palettes data 0 1)
                        gradient
                        Nothing
                    , updateFavicon palette
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
                Just ( gradient, palette ) ->
                    ( Success
                        { palettes | active = index }
                        { gradient | angle = angle }
                        notification
                    , Cmd.batch
                        [ cmd
                        , updateFavicon palette
                        ]
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


delay : Float -> Msg -> Cmd Msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


selectGradient : Int -> List Palette -> Maybe ( Gradient, Palette )
selectGradient index palettes =
    palettes
        |> List.drop index
        |> List.head
        |> Maybe.andThen paletteToGradient


paletteToGradient : Palette -> Maybe ( Gradient, Palette )
paletteToGradient palette =
    let
        colorStops =
            List.map2 Tuple.pair palette.colors (0 :: palette.widths)
                |> List.foldl
                    (\( color, width ) xs ->
                        ( color, widthToPercentage xs width ) :: xs
                    )
                    []
                |> List.reverse
    in
    case colorStops of
        s1 :: s2 :: xs ->
            Just ( Gradient s1 s2 xs 180, palette )

        _ ->
            Nothing


widthToPercentage : List ColorStop -> Float -> Float
widthToPercentage gradient width =
    case gradient of
        ( _, previousWidth ) :: xs ->
            previousWidth + width * 100

        _ ->
            0


totalPages : List a -> Int
totalPages xs =
    ceiling <| toFloat (List.length xs) / toFloat settings.pageSize


paginate : Palettes -> Navigation -> Int
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


navigate : List a -> Int -> Navigation -> ( Int, Cmd Msg )
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
