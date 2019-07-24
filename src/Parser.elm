module Parser exposing (keyDecoder, picturesFromJson, songPackageFromJson, songsFromJson, toPic)

{-| Parses:
the music in songs.json
pictures in pictures.json
FromJsSongPackages which are a Decode.Value
keypresses which are also a Decode.Value
-}

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (required)
import Music exposing (FromJsSongPackage, Song)
import Picture exposing (Picture)



-- decode Keys
-- just returns string of what the user inputted. a spacebar will return " "


keyDecoder =
    Decode.field "key" Decode.string



-- decode pictures


picturesFromJson : Decode.Value -> Result Decode.Error (List Picture)
picturesFromJson jsonString =
    decodeValue string jsonString
        |> Result.andThen toPics


toPics : String -> Result Decode.Error (List Picture)
toPics jsonString =
    decodeString
        (Decode.list picDecoder)
        jsonString


picDecoder : Decoder Picture
picDecoder =
    succeed Picture
        |> required "src" string
        |> required "id" string



-- decode 1 picture


toPic : Decode.Value -> Result Decode.Error Picture
toPic jsonString =
    decodeValue string jsonString
        |> Result.andThen
            (decodeString
                picDecoder
            )



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
