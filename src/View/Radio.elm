module View.Radio exposing (Msg, Radio, getMusic, init, subscriptions, update, view)

{-| Contols music and also controls how it is viewed
-}

import Browser.Events
import Element exposing (Element, Orientation(..), fill, height, width)
import Element.Events as Events
import Element.Font as Font
import FontAwesome.Solid
import Json.Decode
import Music exposing (Music, MusicState(..))
import Ports
import UiUtils.Colors as Colors
import UiUtils.Icon as Icon



-- TYPE


type Radio
    = Radio
        { music : Music
        , orientation : Orientation
        }


init : Music -> Orientation -> Radio
init music orientation =
    Radio
        { music = music
        , orientation = orientation
        }



-- VIEW


view : Radio -> Element Msg
view (Radio data) =
    case data.orientation of
        Landscape ->
            Element.column
                [ width (Element.fillPortion 2)
                , height fill
                ]
                [ Element.el
                    [ height <| Element.fillPortion 2
                    , width fill
                    ]
                    (playPause <| Radio data)
                , Element.column
                    [ width fill
                    , height <| Element.fillPortion 3
                    ]
                    [ songDescription data.music
                    , Element.row
                        [ width fill
                        , height fill
                        , Element.centerX
                        ]
                        [ goBack <| Radio data
                        , goForward <| Radio data
                        ]
                    ]
                ]

        Portrait ->
            Element.column
                [ width fill
                , height (Element.fillPortion 3)
                ]
                [ songDescription data.music
                , Element.row
                    [ width fill
                    , height fill
                    , Element.centerX
                    ]
                    [ goBack <| Radio data
                    , playPause <| Radio data
                    , goForward <| Radio data
                    ]
                ]



-- UPDATE


type Msg
    = RequestNewSong
    | RequestPreviousSong
    | GotNewMusic Music -- once we get the new song, we get returned the entire updated music list
    | ToggleMusic
    | KeyPressed String


update : Msg -> Radio -> ( Radio, Cmd Msg )
update msg (Radio data) =
    case msg of
        RequestNewSong ->
            ( Radio data
            , Music.newSong GotNewMusic data.music
            )

        RequestPreviousSong ->
            ( Radio data
            , Music.previousSong GotNewMusic data.music
            )

        GotNewMusic music ->
            ( Radio { data | music = music }
            , Ports.playMusic ()
            )

        ToggleMusic ->
            let
                musicField =
                    data.music

                newState =
                    Music.toggle musicField.state

                newMusic =
                    { musicField | state = newState }
            in
            ( Radio { data | music = newMusic }
            , Ports.toggleMusic <| Music.stateToBool newState
            )

        KeyPressed value ->
            case value of
                " " ->
                    update ToggleMusic (Radio data)

                _ ->
                    ( Radio data
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions =
    Sub.batch
        [ -- because we need the type on songEnded to be () -> Msg
          Ports.songEnded (\_ -> RequestNewSong)

        -- space bar pauses music. We need to decode the spacebar though rip.
        , Browser.Events.onKeyDown (Json.Decode.map KeyPressed keyDecoder)
        ]


keyDecoder =
    Json.Decode.field "key" Json.Decode.string



-- HELPER FUNCTIONS


getMusic : Radio -> Music
getMusic (Radio data) =
    data.music



-- VIEW HELPERS --


playPause : Radio -> Element Msg
playPause (Radio data) =
    let
        iconWrapper icon =
            Element.el
                [ Element.centerX
                , Element.centerY
                , Font.size 150
                , Events.onClick ToggleMusic
                , Element.pointer
                , Element.mouseDown [ Font.color Colors.light ]
                ]
                (Icon.view icon)
    in
    case data.music.state of
        Music.On ->
            iconWrapper FontAwesome.Solid.pause

        Music.Off ->
            iconWrapper FontAwesome.Solid.play


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
        , Element.centerX
        ]
        (Element.text "Now Playing:")


songName : Music -> Element Msg
songName music =
    Element.paragraph
        [ Font.size 20
        , Font.center
        ]
        [ Element.text music.currentSong.name ]


credit : Music -> Element Msg
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


goBack : Radio -> Element Msg
goBack (Radio data) =
    if List.length data.music.previousSongs == 0 then
        Element.el
            [ width fill, height fill ]
            Element.none

    else
        buttonWrapper RequestPreviousSong (Icon.view FontAwesome.Solid.angleDoubleLeft)


goForward : Radio -> Element Msg
goForward (Radio data) =
    let
        nextSongs =
            data.music.nextSongs
    in
    if List.length nextSongs == 0 then
        buttonWrapper RequestNewSong (Icon.view FontAwesome.Solid.angleDoubleRight)

    else
        buttonWrapper RequestNewSong
            (Icon.withLayer
                { string = String.fromInt <| List.length nextSongs
                , textColor = Colors.dark
                , mainIcon = FontAwesome.Solid.angleDoubleRight
                }
            )


buttonWrapper : Msg -> Element Msg -> Element Msg
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
