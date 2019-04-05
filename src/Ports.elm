port module Ports exposing (confirmCopy, updateFavicon)

import Palette exposing (Palette)


port confirmCopy : (( Bool, String ) -> msg) -> Sub msg


port updateFavicon : Palette -> Cmd msg
