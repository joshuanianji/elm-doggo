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
        , Element.padding 40
        ]
        [ pictureView model
        , musicView model
        ]


pictureView : Model -> Element Msg
pictureView model =
    Element.el
        [ width (Element.fillPortion 2)
        , height fill
        ]
        (Modules.picture model)


musicView : Model -> Element Msg
musicView model =
    case model.music of
        Err errors ->
            Element.column
                []
                [ Element.paragraph
                    [ Font.size 40
                    , Font.bold
                    , Font.center
                    ]
                    [ Element.text "Error! Can't play music. :((" ]
                , Decode.errorToString errors
                    |> Element.text
                    |> List.singleton
                    |> Element.paragraph
                        [ padding 30
                        , Font.family
                            [ Font.typeface "Courier New" ]
                        ]
                ]

        Ok music ->
            musicViewSuccess music


musicViewSuccess : Music -> Element Msg
musicViewSuccess music =
    column
        [ width (Element.fillPortion 2)
        , height fill
        , Element.spacing 50
        ]
        [ soundToggle music
        , radio music
        ]


soundToggle : Music -> Element Msg
soundToggle music =
    let
        iconWrapper icon =
            Element.el
                [ Element.centerX
                , Element.centerY
                , Font.size 150
                , Events.onClick ToggleMusic
                , pointer
                , Element.mouseDown [ Font.color Colors.light ]
                ]
                (Icon.view icon)
    in
    Element.el
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
    Element.column
        [ width fill
        , height (fillPortion 3)
        ]
        [ Modules.songDescription music
        , Modules.skipButtons
        ]
