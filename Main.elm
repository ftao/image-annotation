module Main (..) where

import Effects exposing (Never)


import App exposing (init, update, view, makeInputs)
import Canvas
import Mouse
import StartApp
import Task


app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = makeInputs
    }


main =
  app.html

--port tasks : Signal (Task.Task Never ())
--port tasks =
--  app.tasks
