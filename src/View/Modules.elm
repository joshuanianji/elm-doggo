module View.Modules exposing (Align(..), goBack, goForward, nowPlaying, picture, skipButton, skipButtons, songDescription, songName)

{-| There are key similarities between the view functions of the Portrait and Landscape,
such as the design of the Radio and the picture.
-}

import Element exposing (Element, centerX, centerY, fill, height, pointer, row, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FontAwesome.Solid
import Music exposing (Music)
import Types exposing (Model, Msg(..))
import UiUtils.Colors as Colors
import UiUtils.Icon as Icon exposing (Icon)



-- PICTURE --


picture : Model -> Element Msg
picture model =
    Element.el
        [ Border.rounded 20
        , Border.solid
        , Border.width 22
        , Border.color Colors.lightBlue
        , Background.image model.image
        , width fill
        , height fill
        , pointer
        , Element.mouseDown [ Border.color Colors.light ]
        , Events.onClick ChangePicture
        ]
        Element.none



-- RADIO --


songDescription : Music -> Element Msg
songDescription music =
    Element.column
        [ width fill
        , height fill
        , Element.spacing 15
        , Element.padding 30
        ]
        [ nowPlaying
        , songName music
        , credit music
        ]


nowPlaying : Element Msg
nowPlaying =
    Element.el
        [ Font.size 40
        , Font.bold
        , centerX
        ]
        (Element.text "Now Playing:")


songName : Music -> Element Msg
songName music =
    Element.paragraph
        [ Font.size 20
        , Font.center
        ]
    <|
        case music.currentSong of
            Nothing ->
                [ Element.text "Cannot play current song! :( Please skip to the next song" ]

            Just song ->
                [ Element.text song.name ]


credit : Music -> Element Msg
credit music =
    case music.currentSong of
        Nothing ->
            Element.none

        Just song ->
            Element.paragraph
                [ Font.size 18
                , Font.center
                ]
            <|
                List.singleton <|
                    Element.text <|
                        "(Big thonk to "
                            ++ song.credit
                            ++ ")"


skipButtons : Element Msg
skipButtons =
    row
        [ width fill
        , height fill
        , centerX
        ]
        [ goBack
        , goForward
        ]


goBack : Element Msg
goBack =
    skipButton Right GetPreviousSong FontAwesome.Solid.angleDoubleLeft


goForward : Element Msg
goForward =
    skipButton Left GetNewSong FontAwesome.Solid.angleDoubleRight


skipButton : Align -> Msg -> Icon -> Element Msg
skipButton align onClickEvent icon =
    Element.el
        [ width fill, height fill ]
    <|
        Element.el
            [ centerX
            , centerY
            , Font.size 150
            , pointer
            , Element.mouseDown [ Font.color Colors.light ]
            , Events.onClick onClickEvent
            ]
            (Icon.view icon)


type Align
    = Left
    | Right
