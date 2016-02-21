module Toolbar where
import List
import Maybe exposing (Maybe(Just, Nothing))
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Box

-- MODEL

type AnnotationTool 
   = AnnotationBox
   | AnnotationBlur

type alias Model =
    { tools : List AnnotationTool
    , selected : Maybe AnnotationTool
    }


-- UPDATE

type Action 
    = SelectTool AnnotationTool

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
    case action of
        SelectTool tool -> 
            ( { model | selected = Just tool }
            , Effects.none 
            )
 
-- VIEW

view : Signal.Address Action -> Model -> Html.Html
view address model =
    let 
        toButton : AnnotationTool -> Html
        toButton x =
            button 
                [ class (if (Just x) == model.selected then "active" else "") ] 
                [ text (toString x) ]
    in
        div 
            [class "toolbar"]
            (List.map toButton model.tools)

init : (Model, Effects.Effects Action)
init = 
    ( { tools = 
            [ AnnotationBox
            , AnnotationBlur
            ]
      , selected = Nothing
      }
    , Effects.none
    )
