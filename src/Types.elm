module Types exposing (Flags, Model, Msg(..), WindowSize, init, initModel)

import Element exposing (Device, classifyDevice)
import Json.Decode as Decode
import Music exposing (..)
import Parser
import Picture exposing (Picture, Pictures)
import Ports


type alias Model =
    { device : Device
    , music : Result Decode.Error Music
    , pictures : Result Decode.Error Pictures
    }



-- songsJson is a decode.value because we're going to make sure it's a valid string
-- Therefore we can handle instances where it's null, for example.


type alias Flags =
    { windowSize : WindowSize
    , songsJson : Decode.Value
    , picturesJson : Decode.Value
    }



-- window size.


type alias WindowSize =
    { width : Int
    , height : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        cmdSongs =
            case Parser.songsFromJson flags.songsJson of
                Ok songs ->
                    Ports.getFirstSong songs

                Err _ ->
                    Cmd.none

        cmdPics =
            case Debug.log "pics" <| Parser.picturesFromJson flags.picturesJson of
                Ok pictures ->
                    Ports.gotInitPictures pictures

                Err _ ->
                    Cmd.none
    in
    ( initModel flags
    , Cmd.batch [ cmdSongs, cmdPics ]
    )


initModel : Flags -> Model
initModel flags =
    { device = classifyDevice flags.windowSize
    , music = flags.songsJson |> Parser.songsFromJson |> Result.map Music.init
    , pictures = flags.picturesJson |> Parser.picturesFromJson |> Result.map Picture.init
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
    | GotPicture Decode.Value
