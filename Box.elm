module Box where

-- import Svg exposing (..)
-- import Svg.Attributes exposing (..)

import Html
import Color exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing 
    ( viewBox, width, height, r, cx, cy, x, y, rx, ry
    , stroke, strokeWidth, fill
    )
import Svg.Events exposing (onClick)
import Effects exposing (Effects)

import Element exposing (Point)
import Style exposing (Style)

-- MODEL

type alias Model =
    { p1 : Point
    , p2 : Point
    , isEditing : Bool
    --, style : Style
    }

-- UPDATE

type Action
    = Select
    | UnSelect
    | MoveTopLeft Point
    | MoveTopRight Point
    | MoveBottomLeft Point
    | MoveBottomRight Point

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
        MoveTopLeft point -> 
            ( { model | p1 = point }
            , Effects.none
            )
        MoveTopRight point -> 
            let
                np1  = model.p1
                np2  = model.p2
            in
                ( { model | 
                    p1 = { np1 | y = point.y },
                    p2 = { np2 | x = point.x }
                  }
                , Effects.none
                )
        MoveBottomRight point -> 
            ( { model | p2 = point }
            , Effects.none
            )
        MoveBottomLeft point -> 
            let
                np1  = model.p1
                np2  = model.p2
            in
                ( { model | 
                    p1 = { np1 | x = point.x },
                    p2 = { np2 | y = point.y }
                 }
                , Effects.none
                )
 
-- VIEW

view : Signal.Address Action -> Model -> Html.Html
view address model =
    svg
        [ width "400", height "400", viewBox "0 0 400 400" ]
        ([ (toRect address model) ] ++ (if model.isEditing then (toPoints model) else []))

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
            , fill "none"
            , onClick (Signal.message address Select)
            ] 
            []

toPoints: Model -> List Svg
toPoints model =
    let 
        left = min model.p1.x model.p2.x
        right = max model.p1.x model.p2.x
        top = min model.p1.y model.p2.y
        bottom = max model.p1.y model.p2.y
        makePointShape x y =
            circle 
                [ cx (toString x), cy (toString y), r "5"
                , stroke "black" , strokeWidth "2"
                , fill "none"
                ] 
                []
    in
       [ makePointShape left top
       , makePointShape right top
       , makePointShape right bottom
       , makePointShape left bottom
       ]
        
init = 
    ( { p1 = {x = 100, y = 100}
      , p2 = {x = 300, y = 200}
      , isEditing = False
      }
    , Effects.none
    )
