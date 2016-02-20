module Box where

-- import Svg exposing (..)
-- import Svg.Attributes exposing (..)

import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

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

update : Action -> Model -> Model
update action model =
    case action of 
        Select ->
            { model | isEditing = True }
        UnSelect ->
            { model | isEditing = False }
        MoveTopLeft point -> 
            { model | p1 = point }
        MoveTopRight point -> 
            let
                np1  = model.p1
                np2  = model.p2
            in
                { model | 
                    p1 = { np1 | y = point.y },
                    p2 = { np2 | x = point.x }
                }
        MoveBottomRight point -> 
            { model | p2 = point }
        MoveBottomLeft point -> 
            let
                np1  = model.p1
                np2  = model.p2
            in
                { model | 
                    p1 = { np1 | x = point.x },
                    p2 = { np2 | y = point.y }
                }
 
-- VIEW

view : Model -> Element
view model =
    collage 300 300
       ([ (toRect model) ] ++ (if model.isEditing then (toPoints model) else []))

toRect: Model -> Form
toRect model =
    let 
        w = ((model.p1.x - model.p2.x) |> abs) 
        h = ((model.p1.x - model.p2.x) |> abs) 
        x = (model.p1.x + model.p2.x) / 2
        y = (model.p1.y + model.p2.y) / 2
        outlineStyle = 
            if model.isEditing then {style | color = blue } else style
    in
        rect w h
            |> outlined outlineStyle
            |> move (x, y)

toPoints: Model -> List Form
toPoints model =
    let 
        left = min model.p1.x model.p2.x
        right = max model.p1.x model.p2.x
        top = max model.p1.y model.p2.y
        bottom = min model.p1.y model.p2.y
        pointStyle = { style | color = blue }
        makePointShape x y =
            circle 3 
            |> outlined pointStyle
            |> move (x, y)
    in
       [ makePointShape left top
       , makePointShape right top
       , makePointShape right bottom
       , makePointShape left bottom
       ]
        
style: LineStyle
style = defaultLine

init = 
    { p1 = {x = -100, y = 100}
    , p2 = {x = 100, y = -100}
    , isEditing = False
    }
    
main = view init
