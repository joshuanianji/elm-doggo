module Visual exposing (Visual, VisualType(..), Visuals, getVisualType, init, newVisual)

{-| How I handle Videos and Pictures (I call them Visuals)

    This is much simpler than Music because I'm not implementing backtracking and stuff
    Also pretty funny because I'm also handling videos with this but I'm calling it pictures lol.

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Parser exposing ((|.), (|=), DeadEnd, Parser)
import Random exposing (Generator)
import Random.List
import Set



-- TYPES --


type alias Visuals =
    { current : Visual
    , others : List Visual
    }


type alias Visual =
    { src : String
    , id : String
    }


init : Decode.Value -> Result Decode.Error Visuals
init jsonValue =
    Decode.decodeValue Decode.string jsonValue
        |> Result.andThen (Decode.decodeString visualsDecoder)



-- DECODER


visualsDecoder : Decoder Visuals
visualsDecoder =
    Decode.succeed Visuals
        |> required "current" visualDecoder
        |> required "others" (Decode.list visualDecoder)


visualDecoder : Decoder Visual
visualDecoder =
    Decode.succeed Visual
        |> required "src" Decode.string
        |> required "id" Decode.string



-- RANDOM


newVisual : (Visuals -> msg) -> Visuals -> Cmd msg
newVisual updateMsg visuals =
    Random.generate updateMsg (randomVisualGenerator visuals)


randomVisualGenerator : Visuals -> Generator Visuals
randomVisualGenerator visuals =
    Random.List.choose visuals.others
        |> Random.map
            (\( maybeV, listV ) ->
                case maybeV of
                    Just v ->
                        Visuals v listV

                    -- in the weird case of a visual turning out to be Nothing, I have no backup lmao
                    -- I just keep it as is and log it to the console
                    Nothing ->
                        visuals |> Debug.log "Unable to get next visual"
            )



-- VISUALTYPE


getVisualType : String -> Result (List DeadEnd) VisualType
getVisualType =
    Parser.run visualTypeParser


visualTypeParser : Parser VisualType
visualTypeParser =
    isolateFile
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


isolateFile : Parser String
isolateFile =
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


type VisualType
    = Image
    | Video
