module Canvas (..) where

import Mouse
import Signal
import Task
import Array
import Maybe exposing (Maybe(Just, Nothing))
import Effects exposing (Effects)
import Html
import Svg exposing (..)
import Svg.Events exposing (onClick, onMouseDown, onMouseUp)
import Svg.Attributes exposing (viewBox, width, height, xlinkHref)
import Json.Decode exposing (..)
import Json.Encode

import Element exposing (Point)
import Box
import Mosaic
import Toolbar exposing (AnnotationTool(..))
import Storage


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
  , version: Int
  , savedVersion: Int
  }


encodeAnnotation anno = 
  case anno of 
    AnnoBox x ->
      Box.encodeModel "box" x
    AnnoMosaic x ->
      Mosaic.encodeModel "mosaic" x

annotationDecoder: Decoder Annotation
annotationDecoder =
  ("tag" := string) `andThen` annotationInfo
  
annotationInfo: String -> Decoder Annotation
annotationInfo tag = 
  case tag of 
    "box" ->
      Json.Decode.map AnnoBox Box.modelDecoder
    "mosaic" ->
      Json.Decode.map AnnoMosaic Mosaic.modelDecoder
    _ -> 
      fail (tag ++ " is not a recognized tag for annotation")

encodeModel model = 
  Json.Encode.object 
    [ ("image", Json.Encode.string model.image)
    , ("width", Json.Encode.int model.width)
    , ("height", Json.Encode.int model.height)
    , ("annotations", 
        model.annotations 
          |> Array.map encodeAnnotation 
          |> Json.Encode.array 
      )
    ]

modelDecoder model = 
  Json.Decode.object4
    (\a b c d -> {model | image = a, width = b, height = c, annotations = d})
    ("image" := string)
    ("width" := int)
    ("height" := int)
    ("annotations" := array annotationDecoder)
    

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
  | Select
  | MouseMove Bool Point
  | StartDrag
  | DragTo Point
  | StopDrag
  | Load
  | LoadDone Model
  | Save
  | SaveDone Int
  | NoAction


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
              ( newModel, fx ) =
                applyUpdate act ( index, annotation )
            in
              ( setAnnotation index newModel model
              , fx
              )

          Nothing ->
            ( model, Effects.none )

    AddAnnotation ->
      case model.mousePosition of
        Just point ->
          case model.toolbar.selected of
            Just tool ->
              let
                ( annotation, fx ) =
                  (newAnnotation model.nextId tool point)
              in
                ( { model
                    | annotations = Array.push annotation model.annotations
                    , nextId = (model.nextId + 1)
                  }
                , fx
                )

            _ ->
              ( model, Effects.none )

        _ ->
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

    Select -> 
      forwardAction action model

    DragTo point ->
      forwardAction action model

    StopDrag ->
      forwardAction action model

    StartDrag ->
      ( model, Effects.none )

    Save -> 
      ( model
      , save model
      )
    Load -> 
      ( model
      , load model
      )
    SaveDone version ->
      ( { model 
          | savedVersion = version
        }
      , Effects.none
      )
    LoadDone loadedModel -> 
      (loadedModel, Effects.none)

    NoAction ->
      ( model, Effects.none )

applyUpdate : SubAct -> ( Int, Annotation ) -> ( Annotation, Effects.Effects Action )
applyUpdate act ( index, annotation ) =
  case annotation of
    AnnoBox model ->
      case act of
        BoxAct subAct ->
          Box.update subAct model
            |> mapUpdateResult AnnoBox BoxAct index

        _ ->
          ( annotation, Effects.none )

    AnnoMosaic model ->
      case act of
        MosaicAct subAct ->
          Mosaic.update subAct model
            |> mapUpdateResult AnnoMosaic MosaicAct index

        _ ->
          ( annotation, Effects.none )


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

    Select -> 
      case annotation of
        AnnoBox _ ->
          BoxAct Box.UnSelect

        AnnoMosaic _ ->
          NoAct

    _ ->
      NoAct


forwardAction : Action -> Model -> ( Model, Effects.Effects Action )
forwardAction action model =
  let
    updateResult =
      model.annotations
        |> Array.toIndexedList
        |> List.map (\x -> (applyUpdate (translateAct (snd x) action)) x)

    annotations =
      updateResult
        |> List.map fst
        |> Array.fromList

    effects =
      updateResult
        |> List.map snd
        |> Effects.batch
  in
    ( { model | annotations = annotations }
    , effects
    )


mapUpdateResult : (a -> Annotation) -> (b -> SubAct) -> Int -> ( a, Effects.Effects b ) -> ( Annotation, Effects.Effects Action )
mapUpdateResult func1 func2 index ( m, fx ) =
  ( func1 m
  , Effects.map (\x -> SubMsg index (func2 x)) fx
  )


newAnnotation : Int -> Toolbar.AnnotationTool -> Point -> ( Annotation, Effects.Effects Action )
newAnnotation index tool point =
  case tool of
    AnnotationBox ->
      Box.init point
        |> mapUpdateResult AnnoBox BoxAct index

    AnnotationMosaic ->
      Mosaic.init point
        |> mapUpdateResult AnnoMosaic MosaicAct index



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
        ]
        ([ image
            [ xlinkHref model.image
            , width w
            , height h
            , onMouseDown (Signal.message address AddAnnotation)
            , onMouseUp (Signal.message address StopDrag)
            , onClick (Signal.message address Select)
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
      , Html.button 
         [ onClick (Signal.message address Save) ]
         [ text "Save"]
      , Html.button 
         [ onClick (Signal.message address Load) ]
         [ text "Load"]
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
      , version = 0
      , savedVersion = 0
      }
    , Effects.map Tool toolFx
    )


-- Utils

makeMouseMoveAction : ( Int, Int ) -> Bool -> Action
makeMouseMoveAction pos isDown =
  MouseMove isDown { x = toFloat (fst pos), y = toFloat (snd pos) }

makeMouseStateAction : Bool -> Action
makeMouseStateAction isDown =
  if isDown then
    StartDrag
  else
    StopDrag


makeInputs = 
  [ Signal.map2 makeMouseMoveAction Mouse.position Mouse.isDown
  , Signal.map makeMouseStateAction Mouse.isDown
  ]

shouldRedoable: Action -> Bool
shouldRedoable action = 
  case action of 
    AddAnnotation ->
      True
    Select ->
      True
    _ ->
      False


-- EFFECTS

save : Model -> Effects Action
save model = 
  let 
    result = encodeModel model
      |> Json.Encode.encode 2 
      |> Debug.log "save data"
  in
    Storage.save ("state", result)
      |> Task.succeed 
      |> Task.map (\x -> SaveDone model.version)
      |> Effects.task


load : Model -> Effects Action
load model = 
  let 
    decodeLoadResult x = 
      let
        result = decodeString (modelDecoder model) x
      in 
        case result of
          Ok value ->
            LoadDone value
          Err err -> 
            NoAction
  in 
    Storage.load "state"
      |> Task.map decodeLoadResult
      |> Effects.task
