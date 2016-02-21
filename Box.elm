module Box where

import Debug
import Mouse
import Html
--import Json.Decode as Json exposing((:=))
import Maybe exposing (Maybe(Just, Nothing))
import Color exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing 
    ( viewBox, width, height, r, cx, cy, x, y, rx, ry
    , stroke, strokeWidth, fill
    )
import Svg.Events exposing (onClick, onMouseDown, onMouseUp, onMouseMove)
import Effects exposing (Effects)

import Element exposing (Point)
import Style exposing (Style)

-- MODEL

type alias Model =
    { p1 : Point
    , p2 : Point
    , isEditing : Bool
    , currentPoint : Position
    --, style : Style
    }

type Position = None | TopLeft | TopRight | BottomLeft | BottomRight

-- UPDATE

type Action
    = Select
    | UnSelect
    | SelectPoint Position
    | StopDrag 
    | DragTo Point
--    | MovePoint Position Point


resize: Model -> Position -> Point -> Model
resize model position point =
    case position of 
        None -> 
            model
        TopLeft -> 
            { model | p1 = point }
        TopRight -> 
            let
                np1  = model.p1
                np2  = model.p2
            in
                { model | 
                    p1 = { np1 | y = point.y },
                    p2 = { np2 | x = point.x }
                }
        BottomRight -> 
            { model | p2 = point }
        BottomLeft -> 
            let
                np1  = model.p1
                np2  = model.p2
            in
                { model | 
                    p1 = { np1 | x = point.x },
                    p2 = { np2 | y = point.y }
                }

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
    case action of 
        Select ->
            ( { model | isEditing = True }
            , Effects.none
            )
        UnSelect ->
            ( { model | isEditing = False }
            , Effects.none
            )
        SelectPoint position -> 
            ( { model | currentPoint = position }
            , Effects.none )
        StopDrag -> 
            ( { model | currentPoint = None }
            , Effects.none )
        DragTo point -> 
            ( resize model model.currentPoint point
            , Effects.none )
       
-- VIEW

view : Signal.Address Action -> Model -> Html.Html
view address model =
    svg
        [ width "400", height "400", viewBox "0 0 400 400" ]
        ([ (toRect address model) ] ++ (if model.isEditing then (toPoints address model) else []))

toRect address model =
    let 
        w = (model.p1.x - model.p2.x) |> abs 
        h = (model.p1.y - model.p2.y) |> abs
        minX = min model.p1.x model.p2.x
        minY = min model.p1.y model.p2.y
        --outlineStyle = 
        --    if model.isEditing then {style | color = blue } else style
    in
        rect 
            [ x (toString minX), y (toString minY)
            , width (toString w), height (toString h)
            , rx "5", ry "5"
            , stroke "black", strokeWidth "1"
            , fill "white"
            , onClick (Signal.message address Select)
            ] 
            []

toPoints address model =
    let 
        left = min model.p1.x model.p2.x
        right = max model.p1.x model.p2.x
        top = min model.p1.y model.p2.y
        bottom = max model.p1.y model.p2.y
        makePointShape x y position =
            circle 
                [ cx (toString x), cy (toString y), r "5"
                , stroke "black" , strokeWidth "2"
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
        
init = 
    ( { p1 = {x = 100, y = 100}
      , p2 = {x = 300, y = 200}
      , isEditing = False
      , currentPoint = None
      }
    , Effects.none
    )
