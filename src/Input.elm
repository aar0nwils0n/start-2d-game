module Input exposing (KeyPress, keyPressDecoder)

import Json.Decode as Decode exposing (Decoder)


type alias KeyPress =
    { key : String
    }


keyPressDecoder : Decoder KeyPress
keyPressDecoder =
    Decode.map KeyPress Decode.string
