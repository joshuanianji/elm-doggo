module Visual exposing (Visual, Visuals, init, newVisual)

{-| How I handle Videos and Pictures (I call them Visuals)

    This is much simpler than Music because I'm not implementing backtracking and stuff
    Also pretty funny because I'm also handling videos with this but I'm calling it pictures lol.

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Random exposing (Generator)
import Random.List



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
