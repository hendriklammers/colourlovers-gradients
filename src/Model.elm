module Model exposing
    ( Color
    , ColorStop
    , Gradient
    , Index
    , Model(..)
    , Msg(..)
    , Navigation(..)
    , Notification
    , Palette
    , Palettes
    )

import Http


type Model
    = Init
    | Error String
    | Success Palettes Gradient (Maybe Notification)


type alias Palettes =
    { data : List Palette
    , active : Index
    , page : Index
    }


type alias Notification =
    String


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
    | CopyConfirmation ( Bool, String )
    | CloseNotification
    | NoOp


type Navigation
    = Next
    | Previous
    | Jump Index
    | Random
