module View.Portrait exposing (view)

{-| Picture and radio are in a column
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
    column
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
                [ width fill
                , height <| fillPortion 5
                , padding 40
                ]
                (Modules.picture 40 visuals)

        Err errors ->
            el
                [ height <| fillPortion 5
                , width fill
                ]
            <|
                Modules.errorView "Cannot view picture :(" errors


musicView : Model -> Element Msg
musicView model =
    case model.music of
        Err errors ->
            el
                [ width fill
                , height <| fillPortion 2
                ]
            <|
                Modules.errorView "Cannot play music :(" errors

        Ok music ->
            musicViewSuccess music


musicViewSuccess : Music -> Element Msg
musicViewSuccess music =
    el
        [ height fill
        , width fill
        , spacing 40
        , padding 40
        , centerX
        ]
    <|
        radio music



-- the Now Playing and song title, as well as the skip buttons


radio : Music -> Element Msg
radio music =
    column
        [ height <| fillPortion 3
        , width fill
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
        , Modules.playPause music
        , Modules.goForward music
        ]
