module UiUtils.WindowSize exposing (..)

import Element exposing (Device, Orientation)


type alias WindowSize =
    { width : Int
    , height : Int
    }


toDevice : WindowSize -> Device
toDevice window =
    Element.classifyDevice window


fromXY : Int -> Int -> WindowSize
fromXY =
    WindowSize


orientation : WindowSize -> Orientation
orientation w =
    Element.classifyDevice w
        |> .orientation
