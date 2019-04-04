module Update exposing (Msg(..), Navigation(..))

import Http
import Palette exposing (Palette)


type alias Index =
    Int


type Navigation
    = Next
    | Previous
    | Jump Index
    | Random


type Msg
    = ReceiveData (Result Http.Error (List Palette))
    | Navigate Navigation
    | Paginate Navigation
    | Rotate Float
    | CopyConfirmation ( Bool, String )
    | CloseNotification
    | NoOp
