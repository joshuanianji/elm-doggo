module Main exposing (main)

import Browser
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import FontAwesome.Styles
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode
import Music exposing (Music, MusicState)
import Ports
import UiUtils.Colors as Colors
import View.Radio
import View.Visual
import Visual exposing (Visuals)



-- PROGRAM


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Element.layout []
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { device : Device
    , music : Result Json.Decode.Error Music
    , visuals : Result Json.Decode.Error Visuals
    }



-- songsJson is a decode.value because we're going to make sure it's a valid string
-- Therefore we can handle instances where it's null, for example.


type alias Flags =
    { windowSize : WindowSize
    , songsJson : Json.Decode.Value
    , visualsJson : Json.Decode.Value
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



-- UPDATE


type Msg
    = WindowResize WindowSize
    | ToggleMusic MusicState
    | RequestNewSong
    | RequestPreviousSong
    | GotSong Music -- Acts on newly retrieved song
    | RequestNewVisual
    | GotVisual Visuals
    | KeyPressed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize windowSize ->
            ( { model | device = classifyDevice windowSize |> Debug.log "Device" }
            , Cmd.none
            )

        ToggleMusic currState ->
            let
                updateMusic music =
                    { music | state = Music.toggle currState }
            in
            ( { model | music = Result.map updateMusic model.music }
            , case currState of
                Music.Off ->
                    Ports.toggleMusic True

                -- play music
                Music.On ->
                    Ports.toggleMusic False
              -- pause music
            )

        RequestNewSong ->
            ( model
            , case model.music of
                Ok m ->
                    Music.newSong GotSong m

                _ ->
                    Cmd.none
            )

        RequestPreviousSong ->
            ( model
            , case model.music of
                Ok m ->
                    Music.previousSong GotSong m

                _ ->
                    Cmd.none
            )

        -- once we got a song, and play the music again.
        GotSong music ->
            ( { model | music = Ok music }
            , Ports.playMusic ()
            )

        RequestNewVisual ->
            ( model
            , case model.visuals of
                Ok v ->
                    Visual.newVisual GotVisual v

                _ ->
                    Cmd.none
            )

        GotVisual visuals ->
            ( { model | visuals = Ok visuals }
            , Cmd.none
            )

        KeyPressed value ->
            case value of
                " " ->
                    case model.music of
                        Ok m ->
                            update (ToggleMusic m.state) model

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Element Msg
view model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Font.color Colors.lightBlue
        , Background.color Colors.dark
        ]
        [ FontAwesome.Styles.css |> Element.html -- inject FontAwesome Stylesheet
        , audio model |> Element.html -- hidden element that plays music lol
        , page model
        ]


audio : Model -> Html Msg
audio model =
    Html.div [ Attr.class "elm-audio-player" ]
        [ Html.audio
            [ Attr.src
                (model.music
                    |> Result.map .currentSong
                    |> Result.map .source
                    |> Result.withDefault ""
                )
            , Attr.id "audio-player" -- so the javascript can find it
            ]
            []
        ]


page : Model -> Element Msg
page model =
    (case model.device.orientation of
        Landscape ->
            row
                [ width fill
                , height fill
                ]

        Portrait ->
            column
                [ width fill
                , height fill
                ]
    )
    <|
        [ pictureView model
        , musicView model
        ]


errorView : String -> Json.Decode.Error -> Element Msg
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
        , Json.Decode.errorToString errors
            |> Element.text
            |> List.singleton
            |> Element.paragraph
                [ Element.padding 70
                , Font.family
                    [ Font.typeface "Courier New" ]
                ]
        ]


pictureView : Model -> Element Msg
pictureView model =
    el
        [ width <| fillPortion 3
        , height fill
        , padding 40
        ]
    <|
        case model.visuals of
            Ok visuals ->
                View.Visual.view
                    { visualInfo = visuals
                    , orientation = model.device.orientation
                    , requestNewVisualMsg = RequestNewVisual
                    }

            Err errors ->
                errorView "Cannot view picture :(" errors


musicView : Model -> Element Msg
musicView model =
    case model.music of
        Err errors ->
            el
                [ width <| fillPortion 2 ]
            <|
                errorView "Cannot play music :(" errors

        Ok music ->
            View.Radio.view
                { toggleMsg = ToggleMusic
                , musicInfo = music
                , orientation = model.device.orientation
                , requestNewSongMsg = RequestNewSong
                , requestPreviousSongMsg = RequestPreviousSong
                }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize
            (\x y ->
                WindowResize (WindowSize x y)
            )

        -- because we need the type on songEnded to be () -> Msg
        , Ports.songEnded (\_ -> RequestNewSong)

        -- space bar pauses music. We need to decode the spacebar though rip.
        , Browser.Events.onKeyDown (Json.Decode.map KeyPressed keyDecoder)
        ]


keyDecoder =
    Json.Decode.field "key" Json.Decode.string
