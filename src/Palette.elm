module Palette exposing
    ( Color
    , Palette
    , Palettes
    , getPalettes
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Model exposing (Index, Msg(..))
import Settings exposing (settings)


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


getPalettes : Cmd Msg
getPalettes =
    Http.get
        { url = settings.api
        , expect = Http.expectJson ReceiveData paletteListDecoder
        }


paletteDecoder : Decoder Palette
paletteDecoder =
    Decode.map2 Palette
        (Decode.field "colors" (Decode.list Decode.string))
        (Decode.field "colorWidths" (Decode.list Decode.float))


paletteListDecoder : Decoder (List Palette)
paletteListDecoder =
    Decode.list paletteDecoder
