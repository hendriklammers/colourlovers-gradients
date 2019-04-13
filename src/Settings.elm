module Settings exposing (settings)


settings :
    { api : String
    , pageSize : Int
    , paletteSize : Float
    }
settings =
    { api = "/palettes.json"
    , pageSize = 50
    , paletteSize = 120
    }
