module Palette exposing
    ( Color
    , Index
    , Palette
    , Palettes
    , paletteListDecoder
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Settings exposing (settings)


type alias Index =
    Int


type alias Palettes =
    { data : List Palette
    , active : Index
    , page : Index
    }


type alias Palette =
    { colors : List Color
    , widths : List Float
    }


type alias Color =
    String


paletteDecoder : Decoder Palette
paletteDecoder =
    Decode.map2 Palette
        (Decode.field "colors" (Decode.list Decode.string))
        (Decode.field "colorWidths" (Decode.list Decode.float))


paletteListDecoder : Decoder (List Palette)
paletteListDecoder =
    Decode.list paletteDecoder
