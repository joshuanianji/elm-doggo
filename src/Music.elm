module Music exposing (Music, MusicState(..), Song, init, newSong, previousSong, stateToBool, toggle)

{-| How I handle Music
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Random exposing (Generator)
import Random.List



-- TYPES


type alias Music =
    { previousSongs : List Song
    , currentSong : Song
    , nextSongs : List Song -- the next songs to play (if the user goes back)
    , state : MusicState
    , otherSongs : List Song -- a list of all songs other than the current one (useful for Random generators)
    }


type alias Song =
    { source : String
    , name : String
    , credit : String
    }


type MusicState
    = On
    | Off



-- we make sure it's a valid JSON string first (not a null) then we decode the string in accordance to the jsonInitSongsDecoder


init : Decode.Value -> Result Decode.Error Music
init jsonValue =
    Decode.decodeValue Decode.string jsonValue
        |> Result.andThen (Decode.decodeString jsonInitSongsDecoder)
        |> Result.map
            (\jsonInitSongs ->
                { previousSongs = []
                , currentSong = jsonInitSongs.current
                , nextSongs = []
                , state = Off
                , otherSongs = jsonInitSongs.others
                }
            )


toggle : MusicState -> MusicState
toggle state =
    case state of
        On ->
            Off

        Off ->
            On


stateToBool : MusicState -> Bool
stateToBool state =
    case state of
        On ->
            True

        Off ->
            False



-- DECODE


type alias JsonInitSongs =
    { current : Song
    , others : List Song
    }


jsonInitSongsDecoder : Decoder JsonInitSongs
jsonInitSongsDecoder =
    Decode.succeed JsonInitSongs
        |> required "current" songDecoder
        |> required "others" (Decode.list songDecoder)


songDecoder : Decoder Song
songDecoder =
    Decode.succeed Song
        |> required "source" Decode.string
        |> required "name" Decode.string
        |> required "credit" Decode.string



-- RANDOM


newSong : (Music -> msg) -> Music -> Cmd msg
newSong updateMsg music =
    Random.generate updateMsg (newSongGenerator music)


previousSong : (Music -> msg) -> Music -> Cmd msg
previousSong updateMsg music =
    Random.generate updateMsg (previousSongGenerator music)



-- i should abstract something out of these functions


newSongGenerator : Music -> Generator Music
newSongGenerator music =
    case music.nextSongs of
        x :: xy ->
            Random.constant
                { music
                    | currentSong = x
                    , previousSongs = music.currentSong :: music.previousSongs
                    , nextSongs = xy
                }

        [] ->
            Random.List.choose music.otherSongs
                |> Random.map
                    (\( maybeSong, listSong ) ->
                        case maybeSong of
                            Just song ->
                                { music
                                    | currentSong = song
                                    , previousSongs = music.currentSong :: music.previousSongs
                                    , otherSongs = music.currentSong :: music.otherSongs
                                }

                            -- in the weird case of a music turning out to be Nothing, I have no backup lmao
                            -- I just keep it as is and log it to the console
                            Nothing ->
                                music |> Debug.log "Unable to get next song"
                    )


previousSongGenerator : Music -> Generator Music
previousSongGenerator music =
    case music.previousSongs of
        x :: xy ->
            Random.constant
                { music
                    | currentSong = x
                    , previousSongs = xy
                    , nextSongs = music.currentSong :: music.previousSongs
                }

        [] ->
            Random.List.choose music.otherSongs
                |> Random.map
                    (\( maybeSong, listSong ) ->
                        case maybeSong of
                            Just song ->
                                { music
                                    | currentSong = song
                                    , nextSongs = music.currentSong :: music.nextSongs
                                    , otherSongs = music.currentSong :: music.otherSongs
                                }

                            -- in the weird case of a music turning out to be Nothing, I have no backup lmao
                            -- I just keep it as is and log it to the console
                            Nothing ->
                                music |> Debug.log "Unable to get next song"
                    )
