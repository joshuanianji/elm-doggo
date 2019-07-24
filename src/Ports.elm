port module Ports exposing
    ( getFirstSong
    , getNewSong
    , getPicture
    , getPreviousSong
    , gotInitPictures
    , gotPicture
    , nextSong
    , playMusic
    , songEnded
    , toggleMusic
    )

-- my first time using ports!!

import Json.Decode
import Music exposing (FromJsSongPackage, Song, ToJsSongPackage)
import Picture exposing (Picture, Pictures, ToJsPicPackage)



-- MUSIC --


{-| The boolean is defined as follows:

    True when we want to play

    False when we want to pause

-}
port toggleMusic : Bool -> Cmd msg



-- just wanna play the musicccc
-- also checks if the play button is on play or not (through the autoplay status of the audio tag lmao. Not the best way to do it kek.)


port playMusic : () -> Cmd msg



-- when we initialize the app we only need to get one song from a list of songs
-- . This initializes in the Cmd Msg of our init function


port getFirstSong : List Song -> Cmd msg



-- when we request a new song


port getNewSong : ToJsSongPackage -> Cmd msg



-- when we request a previous song


port getPreviousSong : ToJsSongPackage -> Cmd msg



-- when we get the new song


port nextSong : (Json.Decode.Value -> msg) -> Sub msg



-- javascript will send us this when a song ends, so we can request a new song.
-- I just put a Bool type because idk how Javascript sends an empty tuple


port songEnded : (Bool -> msg) -> Sub msg



-- PICTURES --


port gotInitPictures : List Picture -> Cmd msg


port getPicture : ToJsPicPackage -> Cmd msg


port gotPicture : (Json.Decode.Value -> msg) -> Sub msg
