module Canvas (..) where

import Debug
import Maybe exposing (Maybe(Just, Nothing))
import Effects exposing (Effects)
import Html
import Svg exposing (..)
import Svg.Events exposing (onClick)
import Svg.Attributes exposing (viewBox, width, height, xlinkHref)
import Element exposing (Point)
import Box
import Toolbar exposing (AnnotationTool(..))
import Array


-- MODEL


type Annotation
  = AnnoBox Box.Model


type alias Model =
  { image : String
  , width : Int
  , height : Int
  , toolbar : Toolbar.Model
  , annotations : Array.Array Annotation
  , nextId : Int
  }


setAnnotation index ann model =
    { model | annotations = (Array.set index ann model.annotations) }

-- UPDATE


type Action
  = Tool Toolbar.Action
  | SubBoxMsg Int Box.Action
  | AddAnnotation
  | DragTo Point
  | StopDrag


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    Tool toolact ->
      let
        ( toolbarModel, toolbarFx ) =
          Toolbar.update toolact model.toolbar
      in
        ( { model | toolbar = toolbarModel }
        , Effects.map Tool toolbarFx
        )

    SubBoxMsg index action ->
      let
        aModel = Array.get index model.annotations
      in
        case aModel of
             Just annotation -> 
                case annotation of 
                    AnnoBox boxModel -> 
                         let 
                            (newBox, boxFx) = Box.update action boxModel
                         in
                            ( setAnnotation index (AnnoBox newBox) model
                            , Effects.map (SubBoxMsg index) boxFx
                            )
                    --_ -> 
                    --    (model, Effects.none)
             Nothing -> 
                 (model, Effects.none)

    AddAnnotation ->
      case model.toolbar.selected of
        Just x ->
          let 
            (toolbar, fx) = Toolbar.update Toolbar.UnSelectTool model.toolbar
          in
            ( { model
                | annotations = Array.push (newAnnotation x) model.annotations 
                , nextId = (model.nextId + 1)
                , toolbar = toolbar
                }
            , Effects.none
            )
        Nothing ->
          ( model, Effects.none )

    DragTo point ->
        forwardBoxAction (Box.DragTo point) model
    StopDrag -> 
        forwardBoxAction Box.StopDrag model

forwardBoxAction : Box.Action -> Model -> ( Model, Effects.Effects Action )
forwardBoxAction action model = 
    let 
        applyUpdate : (Int, Annotation) -> 
            {index : Int, model : Annotation, fx : Effects.Effects Action}
        applyUpdate (index, annotation) = 
            case annotation of 
                AnnoBox boxModel -> 
                    let 
                        (newModel, fx) = Box.update action boxModel
                    in 
                        { index = index
                        , model = AnnoBox newModel
                        , fx = Effects.map (SubBoxMsg index) fx
                        }
        updateResult = model.annotations 
            |> Array.toIndexedList
            |> List.map applyUpdate
        annotations = updateResult
            |> List.map .model
            |> Array.fromList
        effects = updateResult 
            |> List.map .fx
            |> Effects.batch
    in
        ( { model | annotations = annotations }
        , effects 
        )

newAnnotation : Toolbar.AnnotationTool -> Annotation
newAnnotation tool =
  case tool of
    AnnotationBox ->
      AnnoBox Box.default

-- VIEW


view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    w =
      toString model.width

    h =
      toString model.height

    toolbar =
      Toolbar.view
        (Signal.forwardTo address Tool)
        model.toolbar

    annotations = model.annotations
       |> Array.toIndexedList
       |> List.map (renderAnnotation address)

    canvas =
      svg
        [ width w
        , height h
        , viewBox ("0 0 " ++ w ++ " " ++ h)
        , onClick (Signal.message address AddAnnotation)
        ]
        ([ image
            [ xlinkHref model.image, width w, height h 
            ]
            []
         ] ++ annotations
        )
  in
    Html.div
      []
      [ toolbar
      , canvas 
      , Html.pre [] [text (toString model)]
      ]


renderAnnotation : Signal.Address Action -> (Int, Annotation) -> Html.Html
renderAnnotation address (index, annotation) =
  case annotation of
    AnnoBox box ->
      Box.view
        (Signal.forwardTo address (SubBoxMsg index))
        box


init : ( Model, Effects.Effects Action )
init =
  let
    ( toolModel, toolFx ) =
      Toolbar.init
  in
    ( { image = "./sample.png"
      , width = 650
      , height = 375
      , toolbar = toolModel
      , annotations = Array.empty
      , nextId = 0
      }
    , Effects.map Tool toolFx
    )
