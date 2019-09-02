module View.Radio exposing (view)

{-| Configure and display a Radio (controls music)
-}

import Element exposing (Element, Orientation(..), fill, height, width)
import Element.Events as Events
import Element.Font as Font
import FontAwesome.Solid
import Music exposing (Music, MusicState(..))
import UiUtils.Colors as Colors
import UiUtils.Icon as Icon


type alias Options msg =
    { toggleMsg : MusicState -> msg
    , musicInfo : Music
    , orientation : Orientation
    , requestNewSongMsg : msg
    , requestPreviousSongMsg : msg
    }


view : Options msg -> Element msg
view options =
    case options.orientation of
        Landscape ->
            Element.column
                [ width (Element.fillPortion 2)
                , height fill
                ]
                [ Element.el
                    [ height <| Element.fillPortion 2
                    , width fill
                    ]
                    (playPause options)
                , Element.column
                    [ width fill
                    , height <| Element.fillPortion 3
                    ]
                    [ songDescription options.musicInfo
                    , Element.row
                        [ width fill
                        , height fill
                        , Element.centerX
                        ]
                        [ goBack options
                        , goForward options
                        ]
                    ]
                ]

        Portrait ->
            Element.column
                [ width fill
                , height (Element.fillPortion 3)
                ]
                [ songDescription options.musicInfo
                , Element.row
                    [ width fill
                    , height fill
                    , Element.centerX
                    ]
                    [ goBack options
                    , playPause options
                    , goForward options
                    ]
                ]



-- HELPERS --


playPause : Options msg -> Element msg
playPause options =
    let
        iconWrapper icon =
            Element.el
                [ Element.centerX
                , Element.centerY
                , Font.size 150
                , Events.onClick (options.toggleMsg options.musicInfo.state)
                , Element.pointer
                , Element.mouseDown [ Font.color Colors.light ]
                ]
                (Icon.view icon)
    in
    case options.musicInfo.state of
        Music.On ->
            iconWrapper FontAwesome.Solid.pause

        Music.Off ->
            iconWrapper FontAwesome.Solid.play


songDescription : Music -> Element msg
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


nowPlaying : Element msg
nowPlaying =
    Element.el
        [ Font.size 40
        , Font.bold
        , Element.centerX
        ]
        (Element.text "Now Playing:")


songName : Music -> Element msg
songName music =
    Element.paragraph
        [ Font.size 20
        , Font.center
        ]
        [ Element.text music.currentSong.name ]


credit : Music -> Element msg
credit music =
    Element.paragraph
        [ Font.size 18
        , Font.center
        ]
    <|
        List.singleton <|
            Element.text <|
                "(Big thonk to "
                    ++ music.currentSong.credit
                    ++ ")"


goBack : Options msg -> Element msg
goBack options =
    if List.length options.musicInfo.previousSongs == 0 then
        Element.el
            [ width fill, height fill ]
            Element.none

    else
        buttonWrapper options.requestPreviousSongMsg (Icon.view FontAwesome.Solid.angleDoubleLeft)


goForward : Options msg -> Element msg
goForward options =
    let
        nextSongs =
            options.musicInfo.nextSongs
    in
    if List.length nextSongs == 0 then
        buttonWrapper options.requestNewSongMsg (Icon.view FontAwesome.Solid.angleDoubleRight)

    else
        buttonWrapper options.requestNewSongMsg
            (Icon.withLayer
                { string = String.fromInt <| List.length nextSongs
                , textColor = Colors.dark
                , mainIcon = FontAwesome.Solid.angleDoubleRight
                }
            )


buttonWrapper : msg -> Element msg -> Element msg
buttonWrapper msg icon =
    Element.el
        [ width fill, height fill ]
    <|
        Element.el
            [ Element.centerX
            , Element.centerY
            , Font.size 150
            , Element.pointer
            , Element.mouseDown [ Font.color Colors.light ]
            , Events.onClick msg
            ]
            icon
