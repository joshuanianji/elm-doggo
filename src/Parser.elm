module Parser exposing (songPackageFromJson, songsFromJson)

{-| Parses the music in songs.json
And parses FromJsSongPackages which are a Decode.Value
-}

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (required)
import Music as Music exposing (FromJsSongPackage, Song)



-- decode the value for FromJsSongPackage


songPackageFromJson : Decode.Value -> Result Decode.Error FromJsSongPackage
songPackageFromJson jsonString =
    decodeValue string jsonString
        |> Result.andThen toPackage


toPackage : String -> Result Decode.Error FromJsSongPackage
toPackage jsonString =
    decodeString packageDecoder jsonString


packageDecoder : Decoder FromJsSongPackage
packageDecoder =
    succeed FromJsSongPackage
        |> required "currentSong" songDecoder
        |> required "previousSongs" (list songDecoder)
        |> required "nextSongs" (list songDecoder)



-- decode the value of Flags.songsJson


songsFromJson : Decode.Value -> Result Decode.Error (List Song)
songsFromJson jsonString =
    decodeValue string jsonString
        |> Result.andThen toSongs


toSongs : String -> Result Decode.Error (List Song)
toSongs jsonString =
    decodeString
        (Decode.list songDecoder)
        jsonString


songDecoder : Decoder Song
songDecoder =
    succeed Song
        |> required "source" string
        |> required "name" string
        |> required "credit" string
