module Element (..) where
import Json.Decode exposing (..)
import Json.Encode 

--- MODEL

type alias Point =
  { x : Float
  , y : Float
  }

pointDecoder = 
  Json.Decode.object2 
    Point ("x" := Json.Decode.float) ("y" := Json.Decode.float)

encodePoint p = 
  Json.Encode.object
    [ ("x", Json.Encode.float p.x)
    , ("y", Json.Encode.float p.y)
    ]

--- UPDATE
