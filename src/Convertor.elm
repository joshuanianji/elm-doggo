module Convertor exposing (getPicType, keyDecoder, picturesFromJson, songPackageFromJson, songsFromJson, toPic)

{-| Can't name it Parser lol, but this module parses:
the music in songs.json
pictures in pictures.json
FromJsSongPackages which are a Decode.Value
keypresses which are also a Decode.Value
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Music exposing (FromJsSongPackage, Song)
import Parser exposing ((|.), (|=), Parser)
import Picture exposing (PicType(..), Picture)
import Set



-- decode Keys
-- just returns string of what the user inputted. a spacebar will return " "


keyDecoder =
    Decode.field "key" Decode.string



-- decode pictures


picturesFromJson : Decode.Value -> Result Decode.Error (List Picture)
picturesFromJson jsonString =
    Decode.decodeValue Decode.string jsonString
        |> Result.andThen toPics


toPics : String -> Result Decode.Error (List Picture)
toPics jsonString =
    Decode.decodeString
        (Decode.list picDecoder)
        jsonString


picDecoder : Decoder Picture
picDecoder =
    Decode.succeed Picture
        |> required "src" Decode.string
        |> required "id" Decode.string



-- decode 1 picture


toPic : Decode.Value -> Result Decode.Error Picture
toPic jsonString =
    Decode.decodeValue Decode.string jsonString
        |> Result.andThen (Decode.decodeString picDecoder)


getPicType : String -> Result (List Parser.DeadEnd) PicType
getPicType url =
    Parser.run picType url |> Debug.log ("getting pic type from " ++ url)



-- get more info out of an image


picType : Parser PicType
picType =
    isolate
        |> Parser.andThen
            (\str ->
                case str of
                    "png" ->
                        Parser.succeed Image

                    "jpg" ->
                        Parser.succeed Image

                    "mp4" ->
                        Parser.succeed Video

                    other ->
                        Parser.problem <| "unknown file type " ++ other
            )



-- isolate the file


isolate : Parser String
isolate =
    Parser.succeed identity
        |. picId
        |. Parser.symbol "."
        |= fileType


fileType : Parser String
fileType =
    Parser.variable
        { start = Char.isLower
        , inner = Char.isAlphaNum
        , reserved = Set.fromList []
        }


picId : Parser ()
picId =
    Parser.chompWhile (\c -> not (c == '.'))



-- decode the value for FromJsSongPackage


songPackageFromJson : Decode.Value -> Result Decode.Error FromJsSongPackage
songPackageFromJson jsonString =
    Decode.decodeValue Decode.string jsonString
        |> Result.andThen toPackage


toPackage : String -> Result Decode.Error FromJsSongPackage
toPackage jsonString =
    Decode.decodeString packageDecoder jsonString


packageDecoder : Decoder FromJsSongPackage
packageDecoder =
    Decode.succeed FromJsSongPackage
        |> required "currentSong" songDecoder
        |> required "previousSongs" (Decode.list songDecoder)
        |> required "nextSongs" (Decode.list songDecoder)



-- decode the value of Flags.songsJson


songsFromJson : Decode.Value -> Result Decode.Error (List Song)
songsFromJson jsonString =
    Decode.decodeValue Decode.string jsonString
        |> Result.andThen toSongs


toSongs : String -> Result Decode.Error (List Song)
toSongs jsonString =
    Decode.decodeString
        (Decode.list songDecoder)
        jsonString


songDecoder : Decoder Song
songDecoder =
    Decode.succeed Song
        |> required "source" Decode.string
        |> required "name" Decode.string
        |> required "credit" Decode.string
