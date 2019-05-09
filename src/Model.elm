module Model exposing
    ( Flags
    , Model(..)
    , Msg(..)
    , Navigation(..)
    , Notification
    , totalPages
    , update
    )

import Gradient exposing (Gradient, fromPalette)
import Http
import Palette exposing (Palette, Palettes)
import Ports exposing (updateFavicon)
import Process
import Random
import Settings exposing (settings)
import Task


type Model
    = Init Flags
    | Error String
    | Success View


type alias Flags =
    { touch : Bool }


type alias View =
    { palettes : Palettes
    , gradient : Gradient
    , notification : Maybe Notification
    , touch : Bool
    }


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
        ( ReceiveData (Ok data), Init { touch } ) ->
            case selectGradient 0 data of
                Just ( gradient, palette ) ->
                    ( Success
                        (View
                            (Palettes data 0 1)
                            gradient
                            Nothing
                            touch
                        )
                    , updateFavicon palette
                    )

                Nothing ->
                    ( Error "Unable to show gradient"
                    , Cmd.none
                    )

        ( ReceiveData (Err _), _ ) ->
            ( Error "Unable to load the color palettes from the server"
            , Cmd.none
            )

        ( viewMsg, Success view ) ->
            updateView viewMsg view

        _ ->
            ( model, Cmd.none )


updateView : Msg -> View -> ( Model, Cmd Msg )
updateView msg ({ palettes, gradient, notification } as view) =
    case msg of
        Navigate nav ->
            let
                ( index, cmd ) =
                    navigate palettes.data palettes.active nav
            in
            case selectGradient index palettes.data of
                Just ( grad, palette ) ->
                    ( Success
                        { view
                            | palettes =
                                { palettes | active = index }
                            , gradient =
                                { gradient | angle = gradient.angle }
                        }
                    , Cmd.batch
                        [ cmd
                        , updateFavicon palette
                        ]
                    )

                Nothing ->
                    ( Error "Unable to show gradient"
                    , Cmd.none
                    )

        Paginate nav ->
            ( Success
                { view
                    | palettes =
                        { palettes | page = paginate palettes nav }
                }
            , Cmd.none
            )

        Rotate angle ->
            ( Success
                { view
                    | gradient =
                        { gradient | angle = gradient.angle + angle }
                }
            , Cmd.none
            )

        CopyConfirmation ( success, _ ) ->
            ( Success
                { view
                    | notification =
                        Just
                            (if success then
                                "Copied CSS code to clipboard."

                             else
                                "Failed to copy CSS code to clipboard"
                            )
                }
            , Cmd.none
            )

        CloseNotification ->
            ( Success { view | notification = Nothing }
            , Cmd.none
            )

        _ ->
            ( Success view, Cmd.none )


delay : Float -> Msg -> Cmd Msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


selectGradient : Int -> List Palette -> Maybe ( Gradient, Palette )
selectGradient index palettes =
    palettes
        |> List.drop index
        |> List.head
        |> Maybe.andThen fromPalette


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
