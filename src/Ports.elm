port module Ports exposing (changePalette, confirmCopy)

import Palette exposing (Palette)


port confirmCopy : (( Bool, String ) -> msg) -> Sub msg


port changePalette : Palette -> Cmd msg
