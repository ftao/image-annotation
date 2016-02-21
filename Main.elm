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
    , inputs = []
    --, inputs = [mouseEvents]
    }


main =
  app.html

{-
mergeMouseEvent: (Int, Int) -> Bool -> Action
mergeMouseEvent pos isDown = 
    if isDown then
        DragTo {x = toFloat (fst pos), y = toFloat (snd pos)}
    else
        StopDrag

mouseEvents = 
    Signal.map2 mergeMouseEvent Mouse.position Mouse.isDown
-}

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

