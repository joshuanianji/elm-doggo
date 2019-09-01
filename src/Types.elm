module Types exposing (Flags, Model, Msg(..), WindowSize, init)

import Element exposing (Device, classifyDevice)
import Json.Decode as Decode
import Music exposing (Music, MusicState)
import Ports
import Visual exposing (Visuals)


type alias Model =
    { device : Device
    , music : Result Decode.Error Music
    , visuals : Result Decode.Error Visuals
    }



-- songsJson is a decode.value because we're going to make sure it's a valid string
-- Therefore we can handle instances where it's null, for example.


type alias Flags =
    { windowSize : WindowSize
    , songsJson : Decode.Value
    , visualsJson : Decode.Value
    }



-- window size.


type alias WindowSize =
    { width : Int
    , height : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { device = classifyDevice flags.windowSize
      , music = Music.init flags.songsJson
      , visuals = Visual.init flags.visualsJson
      }
    , Cmd.none
    )



-- MSG --


type Msg
    = WindowResize WindowSize
    | ToggleMusic MusicState
    | RequestNewSong
    | RequestPreviousSong
    | GotSong Music -- Acts on newly retrieved song
    | RequestNewVisual
    | GotVisual Visuals
    | KeyPressed String
