module Toolbar (..) where

import List
import Maybe exposing (Maybe(Just, Nothing))
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Box


-- MODEL


type AnnotationTool
  = AnnotationBox
  | AnnotationMosaic


type alias Model =
  { tools : List AnnotationTool
  , selected : Maybe AnnotationTool
  }



-- UPDATE


type Action
  = SelectTool AnnotationTool
  | UnSelectTool


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    SelectTool tool ->
      ( { model | selected = Just tool }
      , Effects.none
      )

    UnSelectTool ->
      ( { model | selected = Nothing }
      , Effects.none
      )



-- VIEW


view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    toButton : AnnotationTool -> Html
    toButton x =
      button
        [ class
            (if (Just x) == model.selected then
              "active"
             else
              ""
            )
        , onClick address (SelectTool x)
        ]
        [ text (toString x) ]
  in
    div
      [ class "toolbar" ]
      (List.map toButton model.tools)


init : ( Model, Effects.Effects Action )
init =
  ( { tools =
        [ AnnotationBox
        , AnnotationMosaic
        ]
    , selected = Nothing
    }
  , Effects.none
  )
