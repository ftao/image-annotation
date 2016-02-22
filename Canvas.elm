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


-- MODEL


type Annotation
  = AnnoBox Box.Model


type alias Model =
  { image : String
  , width : Int
  , height : Int
  , toolbar : Toolbar.Model
  , annotations : List Annotation
  }



-- UPDATE


type Action
  = Tool Toolbar.Action
  | SubBoxMsg Box.Action
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

    SubBoxMsg action ->
      ( model, Effects.none )

    DragTo point ->
      ( model, Effects.none )

    StopDrag ->
      ( model, Effects.none )

    AddAnnotation ->
      {-
      ( { model |
          annotations = (model.annotations ++ [(newAnnotation AnnotationBox)])
          }
      , Effects.none)
      -}
      case model.toolbar.selected of
        Just x ->
          ( { model
              | annotations = (model.annotations ++ [ (newAnnotation x) ])
            }
          , Effects.none
          )

        Nothing ->
          ( model, Effects.none )


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

    canvas =
      svg
        [ width w
        , height h
        , viewBox ("0 0 " ++ w ++ " " ++ h)
        , onClick (Signal.message address AddAnnotation)
        ]
        ([ image
            [ xlinkHref model.image, width w, height h ]
            []
         ]
          ++ (List.map (renderAnnotation address) model.annotations)
        )
  in
    Html.div
      []
      [ toolbar, canvas ]


renderAnnotation : Signal.Address Action -> Annotation -> Html.Html
renderAnnotation address annotation =
  case annotation of
    AnnoBox box ->
      Box.view
        (Signal.forwardTo address SubBoxMsg)
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
      , annotations = []
      }
    , Effects.map Tool toolFx
    )
