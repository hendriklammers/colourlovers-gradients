module PaletteTest exposing (suite)

import Expect
import Json.Decode as Decode
import Palette exposing (Palette, paletteDecoder, paletteListDecoder)
import Test exposing (..)


suite : Test
suite =
    describe "Palette module"
        [ decodesJsonPalette
        , decodesJsonList
        ]


decodesJsonList : Test
decodesJsonList =
    test "Decodes json string with list of palettes" <|
        \_ ->
            let
                json =
                    """
[{
    "colors":[
      "69D2E7",
      "A7DBD8",
      "E0E4CC",
      "F38630",
      "FA6900"
    ],
    "colorWidths":[
      0.2,
      0.2,
      0.2,
      0.2,
      0.2
    ],
    "url":"http://www.colourlovers.com/palette/92095/Giant_Goldfish"
  },
  {
    "colors":[
      "FE4365",
      "FC9D9A",
      "F9CDAD",
      "C8C8A9",
      "83AF9B"
    ],
    "colorWidths":[
      0.23,
      0.07,
      0.06,
      0.07,
      0.57
    ],
    "url":"http://www.colourlovers.com/palette/629637/()"
  },
  {
    "colors":[
      "ECD078",
      "D95B43",
      "C02942",
      "542437",
      "53777A"
    ],
    "colorWidths":[
      0.2,
      0.2,
      0.2,
      0.2,
      0.2
    ],
    "url":"http://www.colourlovers.com/palette/694737/Thought_Provoking"
  }]
"""

                palettes =
                    [ { colors =
                            [ "69D2E7"
                            , "A7DBD8"
                            , "E0E4CC"
                            , "F38630"
                            , "FA6900"
                            ]
                      , widths = [ 0.2, 0.2, 0.2, 0.2, 0.2 ]
                      }
                    , { colors =
                            [ "FE4365"
                            , "FC9D9A"
                            , "F9CDAD"
                            , "C8C8A9"
                            , "83AF9B"
                            ]
                      , widths = [ 0.23, 0.07, 0.06, 0.07, 0.57 ]
                      }
                    , { colors =
                            [ "ECD078"
                            , "D95B43"
                            , "C02942"
                            , "542437"
                            , "53777A"
                            ]
                      , widths = [ 0.2, 0.2, 0.2, 0.2, 0.2 ]
                      }
                    ]
            in
            Expect.equal
                (Decode.decodeString paletteListDecoder json)
                (Ok palettes)


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
