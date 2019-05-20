module PaletteTest exposing (suite)

import Expect
import Json.Decode as Decode
import Palette exposing (Palette, paletteDecoder)
import Test exposing (..)


suite : Test
suite =
    describe "Palette module"
        [ decodesJsonPalette
        ]


decodesJsonPalette : Test
decodesJsonPalette =
    test "Decodes json string into valid Palette" <|
        \_ ->
            let
                json =
                    """
                    {"colors":["69D2E7","A7DBD8","E0E4CC","F38630","FA6900"],"colorWidths":[0.2,0.2,0.2,0.2,0.2],"url":"http://www.colourlovers.com/palette/92095/Giant_Goldfish"}
"""

                palette =
                    { colors =
                        [ "69D2E7"
                        , "A7DBD8"
                        , "E0E4CC"
                        , "F38630"
                        , "FA6900"
                        ]
                    , widths = [ 0.2, 0.2, 0.2, 0.2, 0.2 ]
                    }
            in
            Expect.equal
                (Decode.decodeString paletteDecoder json)
                (Ok palette)
