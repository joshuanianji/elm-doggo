module Music exposing (FromJsSongPackage, Music, MusicState(..), Song, ToJsSongPackage, init, toBool, toggle)

{-| How I handle Music
-}

import Json.Decode as Decode



-- I include previous songs so I can go back.


type alias Music =
    { previousSongs : List Song
    , currentSong : Maybe Song -- in case we can't find the song or something (??)
    , nextSongs : List Song -- the next songs to play (if the user goes back)
    , state : MusicState
    , songs : List Song -- a list of all songs
    }


init : List Song -> Music
init songs =
    { previousSongs = []
    , currentSong = Nothing
    , nextSongs = []
    , state = Off
    , songs = songs
    }


type MusicState
    = On
    | Off


toggle : MusicState -> MusicState
toggle state =
    case state of
        On ->
            Off

        Off ->
            On


toBool : MusicState -> Bool
toBool state =
    case state of
        On ->
            True

        Off ->
            False



-- data format we send to JS


type alias ToJsSongPackage =
    { previousSongs : List Song
    , currentSong : Song
    , nextSongs : List Song
    , allSongs : List Song
    }



-- data format for when javascript sends us back information


type alias FromJsSongPackage =
    { currentSong : Song
    , previousSongs : List Song
    , nextSongs : List Song
    }


type alias Song =
    { source : String
    , name : String
    , credit : String
    }
