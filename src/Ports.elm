port module Ports exposing
    ( playMusic
    , songEnded
    , toggleMusic
    )

-- my first time using ports!!

import Json.Decode
import Music exposing (Song)
import Visual exposing (Visual, Visuals)



{-| The boolean is defined as follows:

    True when we want to play

    False when we want to pause

-}
port toggleMusic : Bool -> Cmd msg



-- just wanna play the musicccc
-- also checks if the play button is on play or not (through the autoplay status of the audio tag lmao. Not the best way to do it kek.)


port playMusic : () -> Cmd msg



-- javascript will send us this when a song ends, so we can request a new song.
-- I just put a Bool type because idk how Javascript sends an empty tuple


port songEnded : (Bool -> msg) -> Sub msg
