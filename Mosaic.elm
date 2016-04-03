module Mosaic (..) where
import Mouse
import Html
import Maybe exposing (Maybe(Just, Nothing), withDefault)
import Color exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (id, viewBox, width, height, r, in', stdDeviation, cx, cy, x, y, rx, ry, stroke, strokeWidth, fill)
import Svg.Events exposing (onClick, onMouseDown, onMouseUp, onMouseMove)
import Effects exposing (Effects)
import Element exposing (Point, pointDecoder, encodePoint)
import Style exposing (Style)
import Json.Decode exposing (..)
import Json.Encode

-- MODEL

type alias Model =
  { p1 : Point
  , p2 : Point
  , isEditing :
      Bool
      --, style : Style
  }


modelDecoder = 
  Json.Decode.object2 (\p1 p2 -> Model p1 p2 False)
    ("p1" := pointDecoder)
    ("p2" := pointDecoder)


encodeModel tag model = 
  Json.Encode.object 
    [ ("tag", Json.Encode.string tag)
    , ("p1", encodePoint model.p1)
    , ("p2", encodePoint model.p2)
    ]


-- UPDATE


type Action
  = StopDrag
  | DragTo Point


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    DragTo point ->
      if model.isEditing then
        ( { model | p2 = point }
        , Effects.none
        )
      else
        ( model, Effects.none )

    StopDrag ->
      ( { model | isEditing = False }
      , Effects.none
      )



-- VIEW


view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    filterName =
      "f1"
  in
    g
      []
      [ (toFilter address model filterName)
      , (toRect address model filterName)
      ]


toFilter address model filterName =
  defs
    []
    [ filter
        [ id filterName
        , x "0"
        , y "0"
        ]
        [ feGaussianBlur
            [ in' "SourceGraphic"
            , stdDeviation "15"
            ]
            []
        ]
    ]


toRect address model filterName =
  let
    w =
      (model.p1.x - model.p2.x) |> abs

    h =
      (model.p1.y - model.p2.y) |> abs

    minX =
      min model.p1.x model.p2.x

    minY =
      min model.p1.y model.p2.y
  in
    rect
      [ x (toString minX)
      , y (toString minY)
      , width (toString w)
      , height (toString h)
      , stroke "none"
      , fill "white"
      , Svg.Attributes.filter ("url(#" ++ filterName ++ ")")
      ]
      []



-- Init


init : Point -> ( Model, Effects.Effects Action )
init p =
  let
    defaultWidth =
      1.0

    defaultHeight =
      1.0

    model =
      { p1 = p
      , p2 = Point (p.x + defaultWidth) (p.y + defaultHeight)
      , isEditing = True
      }
  in
    ( model, Effects.none )
