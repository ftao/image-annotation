module Main (..) where

import Effects exposing (Never)


--import Box exposing (init, update, view, Action(..))

import Canvas exposing (init, update, view, Action(..))
import Mouse
import StartApp
import Task


app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = [ mouseMoveEvents, mouseStateEvents ]
    }


main =
  app.html


mergeMouseEvent : ( Int, Int ) -> Bool -> Action
mergeMouseEvent pos isDown =
  MouseMove isDown { x = toFloat (fst pos), y = toFloat (snd pos) }


makeMouseStateEvent : Bool -> Action
makeMouseStateEvent isDown =
  if isDown then
    StartDrag
  else
    StopDrag


mouseMoveEvents =
  Signal.map2 mergeMouseEvent Mouse.position Mouse.isDown


mouseStateEvents =
  Signal.map makeMouseStateEvent Mouse.isDown


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
