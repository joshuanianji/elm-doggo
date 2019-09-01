module View.Landscape exposing (view)

{-| Displays the picture beside the radio (audio controls)
-}

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FontAwesome.Solid
import Html
import Music exposing (Music)
import Types exposing (Model, Msg(..))
import UiUtils.Colors as Colors
import UiUtils.Icon as Icon exposing (Icon)
import View.Modules as Modules


view : Model -> Element Msg
view model =
    row
        [ width fill
        , height fill
        ]
        [ pictureView model
        , musicView model
        ]


pictureView : Model -> Element Msg
pictureView model =
    case model.visuals of
        Ok visuals ->
            el
                [ width <| fillPortion 3
                , height fill
                , padding 40
                ]
                (Modules.picture 22 visuals)

        Err errors ->
            el
                [ width <| fillPortion 3 ]
            <|
                Modules.errorView "Cannot view picture :(" errors


musicView : Model -> Element Msg
musicView model =
    case model.music of
        Err errors ->
            el
                [ width <| fillPortion 2 ]
            <|
                Modules.errorView "Cannot play music :(" errors

        Ok music ->
            musicViewSuccess music


musicViewSuccess : Music -> Element Msg
musicViewSuccess music =
    column
        [ width <| fillPortion 2
        , height fill
        , spacing 40
        , padding 40
        ]
        [ musicToggle music
        , radio music
        ]


musicToggle : Music -> Element Msg
musicToggle music =
    el
        [ height <| fillPortion 2
        , width fill
        ]
    <|
        Modules.playPause music



-- the Now Playing and song title, as well as the skip buttons


radio : Music -> Element Msg
radio music =
    column
        [ width fill
        , height <| fillPortion 3
        ]
        [ Modules.songDescription music
        , skipButtons music
        ]


skipButtons : Music -> Element Msg
skipButtons music =
    row
        [ width fill
        , height fill
        , centerX
        ]
        [ Modules.goBack music
        , Modules.goForward music
        ]
