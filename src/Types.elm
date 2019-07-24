module Types exposing (Flags, Model, Msg(..), WindowSize, init, initModel)

import Element exposing (Device, classifyDevice)
import Json.Decode as Decode
import Music exposing (..)
import Parser
import Ports


type alias Model =
    { device : Device
    , image : String
    , music : Result Decode.Error Music
    }



-- songsJson is a decode.value because we're going to make sure it's a valid string
-- Therefore we can handle instances where it's null, for example.


type alias Flags =
    { windowSize : WindowSize
    , songsJson : Decode.Value
    }



-- window size.


type alias WindowSize =
    { width : Int
    , height : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        cmd =
            case Parser.songsFromJson flags.songsJson of
                Ok songs ->
                    Ports.getFirstSong songs

                Err _ ->
                    Cmd.none
    in
    ( initModel flags
    , cmd
    )


initModel : Flags -> Model
initModel flags =
    { device = classifyDevice flags.windowSize
    , image = "https://cdn1.medicalnewstoday.com/content/images/articles/322/322868/golden-retriever-puppy.jpg"
    , music = flags.songsJson |> Parser.songsFromJson |> Result.map Music.init
    }



-- MSG --


type Msg
    = WindowResize WindowSize
    | GetNewSong -- request a new song
    | GetPreviousSong -- requests a previous song
    | GotSong Decode.Value -- Javascript returns to us data for a new song
    | ToggleMusic
    | ChangePicture
    | KeyPressed String
