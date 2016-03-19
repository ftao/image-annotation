module App (..) where

import Set
import Keyboard
import Char
import Effects exposing (Effects)
import Html
import UndoList
import Canvas

-- MODEL

type alias Model = 
  UndoList.UndoList Canvas.Model


-- UPDATE

type alias Action = UndoList.Action Canvas.Action

update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of 
    UndoList.New act ->
      let 
        (new_state, effect) = Canvas.update act model.present
        new_model = 
          if Canvas.shouldRedoable act then
            UndoList.new new_state model
          else
            { model | present = new_state}
      in 
        ( new_model
        , Effects.map UndoList.New effect
        )

    UndoList.Reset ->
      ( UndoList.reset model
      , Effects.none
      )

    UndoList.Redo  ->
      ( UndoList.redo model
      , Effects.none
      )

    UndoList.Undo  ->
      ( UndoList.undo model
      , Effects.none
      )

    UndoList.Forget ->
      ( UndoList.forget model
      , Effects.none
      )

-- VIEW

view : Signal.Address Action -> Model -> Html.Html
view address model = 
  Canvas.view 
    (Signal.forwardTo address UndoList.New)
    model.present 

-- INIT

init : ( Model, Effects.Effects Action )
init =
  let
    ( state, fx ) = Canvas.init
  in
    ( UndoList.fresh state
    , Effects.map UndoList.New fx
    )


-- INPUTS

makeKeybordShortAction ctrlKey keys = 
  let 
    rKeyDown = Set.member (Char.toCode 'R') keys
    zKeyDown = Set.member (Char.toCode 'Z') keys
  in 
    if ctrlKey && zKeyDown then
      UndoList.Undo
    else if ctrlKey && rKeyDown then
      UndoList.Redo
    else
      UndoList.New Canvas.NoAction

makeInputs = 
  let 
    inputsForCanvas 
      = List.map
          (Signal.map UndoList.New)
          Canvas.makeInputs
    inputsForRedo
      = [ Signal.map2
            makeKeybordShortAction
            Keyboard.ctrl
            Keyboard.keysDown
        ]
  in
    inputsForCanvas ++ inputsForRedo
