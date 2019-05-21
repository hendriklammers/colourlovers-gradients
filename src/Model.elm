module Model exposing
    ( Flags
    , Model(..)
    , Msg(..)
    , Navigation(..)
    , Notification(..)
    , Touch
    , View
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
    { touch : Bool
    , online : Bool -- Unused right now
    }


type alias View =
    { palettes : Palettes
    , gradient : Gradient
    , notification : Notification
    , touch : Touch
    }


type alias Touch =
    Bool


type Notification
    = Closed
    | Message String


type Msg
    = ReceiveData (Result Http.Error (List Palette))
    | Navigate Navigation
    | RandomPalette
    | Paginate Navigation
    | Rotate Float
    | CopyConfirmation ( Bool, String )
    | CloseNotification
    | NoOp


type Navigation
    = Next
    | Previous
    | Jump Int


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
                            Closed
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
updateView msg ({ palettes, gradient } as view) =
    case msg of
        Navigate nav ->
            let
                index =
                    navigate palettes.data palettes.active nav
            in
            case selectGradient index palettes.data of
                Just ( newGradient, palette ) ->
                    ( Success
                        { view
                            | palettes =
                                { palettes | active = index }
                            , gradient =
                                { newGradient | angle = gradient.angle }
                        }
                    , updateFavicon palette
                    )

                Nothing ->
                    ( Error "Unable to show gradient"
                    , Cmd.none
                    )

        RandomPalette ->
            ( Success view
            , Random.generate
                (\i -> Navigate (Jump i))
                (Random.int 0 (List.length <| palettes.data))
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
                        Message
                            (if success then
                                "Copied CSS code to clipboard."

                             else
                                "Failed to copy CSS code to clipboard"
                            )
                }
            , delay 1500 CloseNotification
            )

        CloseNotification ->
            ( Success { view | notification = Closed }
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


navigate : List a -> Int -> Navigation -> Int
navigate xs current nav =
    case nav of
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
