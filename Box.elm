module Box (..) where

import Maybe exposing (..)
import Mouse
import Html
import Maybe exposing (Maybe(Just, Nothing), withDefault)
import Color exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (viewBox, width, height, r, cx, cy, x, y, rx, ry, stroke, strokeWidth, fill)
import Svg.Events exposing (onClick, onMouseDown, onMouseUp, onMouseMove)
import Effects exposing (Effects)
import Element exposing (Point, pointDecoder, encodePoint)
import Style exposing (Style)
import Json.Decode exposing (..)
import Json.Encode


-- MODEL

type State
  = Creating 
  | Editing
  | Normal

type alias Model =
  { p1 : Point
  , p2 : Point
  , state : State
  , currentPoint :
      Position
      --, style : Style
  }


type Position
  = None
  | TopLeft
  | TopRight
  | BottomLeft
  | BottomRight


modelDecoder = 
  object2 (\p1 p2 -> Model p1 p2 Normal None)
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
  = Select
  | UnSelect
  | SelectPoint Position
  | StopDrag
  | DragTo Point


resize : Model -> Position -> Point -> Model
resize model position point =
  case position of
    None ->
      model

    TopLeft ->
      { model | p1 = point }

    TopRight ->
      let
        np1 =
          model.p1

        np2 =
          model.p2
      in
        { model
          | p1 = { np1 | y = point.y }
          , p2 = { np2 | x = point.x }
        }

    BottomRight ->
      { model | p2 = point }

    BottomLeft ->
      let
        np1 =
          model.p1

        np2 =
          model.p2
      in
        { model
          | p1 = { np1 | x = point.x }
          , p2 = { np2 | y = point.y }
        }


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    Select ->
      ( { model | state = Editing }
      , Effects.none
      )

    UnSelect ->
      ( { model 
            | state = Normal 
            , currentPoint = None
        }
      , Effects.none
      )

    SelectPoint position ->
      ( { model | currentPoint = position }
      , Effects.none
      )

    StopDrag ->
      case model.state of 
        Creating -> 
          ( { model 
                | state = Normal 
                , currentPoint = None 
            }
          , Effects.none
          )
        Editing -> 
          ( { model | currentPoint = None }
          , Effects.none
          )
        Normal -> 
          ( model, Effects.none)

    DragTo point ->
      case model.state of 
        Creating -> 
          ( resize model BottomRight point
          , Effects.none
          )
        Editing -> 
          ( resize model model.currentPoint point
          , Effects.none
          )
        Normal -> 
          ( model, Effects.none )


-- VIEW


view : Signal.Address Action -> Model -> Html.Html
view address model =
  g
    [ onMouseUp (Signal.message address StopDrag)
    ]
    ([ (toRect address model) ]
      ++ (if model.state == Editing then
            (toPoints address model)
          else
            []
         )
    )


toRect address model =
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
      , rx "5"
      , ry "5"
      , stroke "black"
      , strokeWidth "2"
      , fill "transparent"
      , onClick (Signal.message address Select)
      ]
      []


toPoints address model =
  let
    left =
      min model.p1.x model.p2.x

    right =
      max model.p1.x model.p2.x

    top =
      min model.p1.y model.p2.y

    bottom =
      max model.p1.y model.p2.y

    makePointShape x y position =
      circle
        [ cx (toString x)
        , cy (toString y)
        , r "5"
        , stroke "black"
        , strokeWidth "2"
        , fill "blue"
        , onMouseDown (Signal.message address (SelectPoint position))
        ]
        []
  in
    [ makePointShape left top TopLeft
    , makePointShape right top TopRight
    , makePointShape right bottom BottomRight
    , makePointShape left bottom BottomLeft
    ]



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
      , state = Creating
      , currentPoint = None
      }
  in
    ( model, Effects.none )
