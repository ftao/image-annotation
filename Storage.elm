module Storage(save, load) where 
import Task exposing (Task)
import Native.LocalStorage

save : (String, String) -> Task x ()
save =
  Native.LocalStorage.save

load : String -> Task x String
load =
  Native.LocalStorage.load

