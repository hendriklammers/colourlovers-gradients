port module Ports exposing (confirmCopy)


port confirmCopy : (( Bool, String ) -> msg) -> Sub msg
