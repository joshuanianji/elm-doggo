module View.Landscape exposing (musicView, pictureView, view)

{-| Displays the picture beside the radio (audio controls)
-}

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FontAwesome.Solid
import Html
import Json.Decode as Decode
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
    case model.pictures of
        Ok pictures ->
            el
                [ width (fillPortion 3)
                , height fill
                , padding 40
                ]
                (Modules.picture pictures)

        Err errors ->
            el
                [ width (fillPortion 3) ]
            <|
                errorView "Cannot view picture :(" errors


musicView : Model -> Element Msg
musicView model =
    case model.music of
        Err errors ->
            el
                [ width (fillPortion 2) ]
            <|
                errorView "Cannot view picture :(" errors

        Ok music ->
            musicViewSuccess music


musicViewSuccess : Music -> Element Msg
musicViewSuccess music =
    column
        [ width (fillPortion 2)
        , height fill
        , spacing 40
        , padding 40
        ]
        [ soundToggle music
        , radio music
        ]


soundToggle : Music -> Element Msg
soundToggle music =
    let
        iconWrapper icon =
            el
                [ centerX
                , centerY
                , Font.size 150
                , Events.onClick ToggleMusic
                , pointer
                , mouseDown [ Font.color Colors.light ]
                ]
                (Icon.view icon)
    in
    el
        [ height (fillPortion 2)
        , width fill
        ]
    <|
        case music.state of
            Music.On ->
                iconWrapper FontAwesome.Solid.pause

            Music.Off ->
                iconWrapper FontAwesome.Solid.play



-- the Now Playing and song title, as well as the skip buttons


radio : Music -> Element Msg
radio music =
    column
        [ width fill
        , height (fillPortion 3)
        ]
        [ Modules.songDescription music
        , Modules.skipButtons music
        ]



-- helpers


errorView : String -> Decode.Error -> Element Msg
errorView awwMan errors =
    Element.column
        [ height fill
        , Element.spacing 40
        ]
        [ Element.paragraph
            [ Font.size 70
            , Font.bold
            , Font.center
            ]
            [ Element.text awwMan ]
        , Decode.errorToString errors
            |> Element.text
            |> List.singleton
            |> Element.paragraph
                [ Element.padding 70
                , Font.family
                    [ Font.typeface "Courier New" ]
                ]
        ]
