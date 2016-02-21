module Canvas where
import Maybe exposing (Maybe(Just, Nothing))
import Effects exposing (Effects)
import Html
import Svg exposing (..)
import Svg.Attributes exposing 
    ( viewBox, width, height
    , xlinkHref
    )
import Box
import Toolbar

-- MODEL

type Annotation 
   = AnnotateBox Box.Model

type AnnotationType 
   = AnnotationBox

type alias Model =
    { image : String
    , width : Int
    , height : Int
    , toolbar : Toolbar.Model
--    , annotations : List Annotation
--    , tools : List AnnotateType
--    , selectedTool: Maybe AnnotationType
    }


-- UPDATE

type Action 
    = Tool Toolbar.Action

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
    case action of
        Tool action -> 
            (model, Effects.none)
 

-- VIEW

view : Signal.Address Action -> Model -> Html.Html
view address model =
    let 
        w = toString model.width
        h = toString model.height
        toolbar = Toolbar.view 
            (Signal.forwardTo address Tool)
            model.toolbar
        canvas = 
            svg 
                [ width w, height h, viewBox ("0 0 " ++ w ++ " " ++ h) ]
                [ image 
                    [ xlinkHref model.image, width w, height h]
                    []
                ]
    in 
       Html.div 
          []
          [ toolbar, canvas ]

init : (Model, Effects.Effects Action)
init = 
    let 
        (toolModel, toolFx) = Toolbar.init
    in
        ( { image = "./sample.png"
        , width = 650
        , height = 375
        , toolbar = toolModel
        }
        , Effects.batch
            [ Effects.map Tool toolFx
            ]
        )
