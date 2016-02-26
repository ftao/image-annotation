module Canvas (..) where

import Debug
import Maybe exposing (Maybe(Just, Nothing))
import Effects exposing (Effects)
import Html
import Svg exposing (..)
import Svg.Events exposing (onClick, onMouseDown, onMouseUp)
import Svg.Attributes exposing (viewBox, width, height, xlinkHref)
import Element exposing (Point)
import Box
import Mosaic
import Toolbar exposing (AnnotationTool(..))
import Array


-- MODEL


type Annotation
  = AnnoBox Box.Model
  | AnnoMosaic Mosaic.Model


type alias Model =
  { image : String
  , width : Int
  , height : Int
  , toolbar : Toolbar.Model
  , annotations : Array.Array Annotation
  , nextId : Int
  , mousePosition : Maybe Point
  }


setAnnotation index ann model =
  { model | annotations = (Array.set index ann model.annotations) }



-- UPDATE


type SubAct
  = BoxAct Box.Action
  | MosaicAct Mosaic.Action
  | NoAct


type Action
  = Tool Toolbar.Action
  | SubMsg Int SubAct
  | AddAnnotation
  | MouseMove Bool Point
  | StartDrag
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

    SubMsg index act ->
      let
        aModel =
          Array.get index model.annotations
      in
        case aModel of
          Just annotation ->
            let
              result =
                applyUpdate act ( index, annotation )
            in
              ( setAnnotation index result.model model
              , result.fx
              )

          Nothing ->
            ( model, Effects.none )

    AddAnnotation ->
      case model.toolbar.selected of
        Just x ->
          let
            ( toolbar, fx ) =
              Toolbar.update Toolbar.UnSelectTool model.toolbar
          in
            ( { model
                | annotations = Array.push (newAnnotation x model.mousePosition) model.annotations
                , nextId = (model.nextId + 1)
                , toolbar = toolbar
              }
            , Effects.none
            )

        Nothing ->
          ( model, Effects.none )

    MouseMove isDown point ->
      let
        model =
          { model | mousePosition = Just point }
      in
        if isDown then
          forwardAction (DragTo point) model
        else
          ( model, Effects.none )

    DragTo point ->
      forwardAction action model

    StopDrag ->
      forwardAction action model

    StartDrag ->
      ( model, Effects.none )


applyUpdate : SubAct -> ( Int, Annotation ) -> { index : Int, model : Annotation, fx : Effects.Effects Action }
applyUpdate act ( index, annotation ) =
  case annotation of
    AnnoBox model ->
      case act of
        BoxAct boxAct ->
          let
            ( newModel, fx ) =
              Box.update boxAct model
          in
            { index = index
            , model = AnnoBox newModel
            , fx = Effects.map (\x -> (SubMsg index (BoxAct x))) fx
            }

        _ ->
          { index = index
          , model = annotation
          , fx = Effects.none
          }

    AnnoMosaic mosaicModel ->
      case act of
        MosaicAct mosaicAct ->
          let
            ( newModel, fx ) =
              Mosaic.update mosaicAct mosaicModel
          in
            { index = index
            , model = AnnoMosaic newModel
            , fx = Effects.map (\x -> (SubMsg index (MosaicAct x))) fx
            }

        _ ->
          { index = index
          , model = annotation
          , fx = Effects.none
          }


translateAct : Annotation -> Action -> SubAct
translateAct annotation action =
  case action of
    DragTo point ->
      case annotation of
        AnnoBox _ ->
          BoxAct (Box.DragTo point)

        AnnoMosaic _ ->
          MosaicAct (Mosaic.DragTo point)

    StopDrag ->
      case annotation of
        AnnoBox _ ->
          BoxAct Box.StopDrag

        AnnoMosaic _ ->
          MosaicAct Mosaic.StopDrag

    _ ->
      NoAct


forwardAction : Action -> Model -> ( Model, Effects.Effects Action )
forwardAction action model =
  let
    updateResult =
      model.annotations
        |> Array.toIndexedList
        |> List.map (\x -> applyUpdate (translateAct (snd x) action) x)

    annotations =
      updateResult
        |> List.map .model
        |> Array.fromList

    effects =
      updateResult
        |> List.map .fx
        |> Effects.batch
  in
    ( { model | annotations = annotations }
    , effects
    )


newAnnotation : Toolbar.AnnotationTool -> Maybe Point -> Annotation
newAnnotation tool point =
  case tool of
    AnnotationBox ->
      AnnoBox (Box.withStartPoint point)

    AnnotationMosaic ->
      AnnoMosaic (Mosaic.withStartPoint point)



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

    annotations =
      model.annotations
        |> Array.toIndexedList
        |> List.map (renderAnnotation address)

    canvas =
      svg
        [ width w
        , height h
        , viewBox ("0 0 " ++ w ++ " " ++ h)
        , onMouseDown (Signal.message address AddAnnotation)
        , onMouseUp (Signal.message address StopDrag)
        ]
        ([ image
            [ xlinkHref model.image
            , width w
            , height h
            ]
            []
         ]
          ++ annotations
        )
  in
    Html.div
      []
      [ toolbar
      , canvas
      , Html.pre [] [ text (toString model) ]
      ]


renderAnnotation : Signal.Address Action -> ( Int, Annotation ) -> Html.Html
renderAnnotation address ( index, annotation ) =
  case annotation of
    AnnoBox model ->
      Box.view
        (Signal.forwardTo address (\x -> SubMsg index (BoxAct x)))
        model

    AnnoMosaic model ->
      Mosaic.view
        (Signal.forwardTo address (\x -> SubMsg index (MosaicAct x)))
        model


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
      , mousePosition = Nothing
      }
    , Effects.map Tool toolFx
    )
