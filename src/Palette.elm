module Palette exposing
    ( Color
    , Palette
    , Palettes
    , paletteDecoder
    , paletteListDecoder
    )

import Json.Decode as Decode exposing (Decoder)


type alias Palettes =
    { data : List Palette
    , active : Int
    , page : Int
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
