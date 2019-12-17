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
import Modules.Radio exposing (Radio)
import Modules.Visual exposing (Visual)
import Music exposing (Music, MusicState)
import Ports
import UiUtils.Colors as Colors
import UiUtils.WindowSize as WindowSize exposing (WindowSize)
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

    -- if the music is wack, the errors get propagated to this Result type
    , radio : Result Json.Decode.Error Radio
    , visual : Result Json.Decode.Error Visual
    }



-- songsJson is a decode.value because we're going to make sure it's a valid string
-- Therefore we can handle instances where it's null, for example.


type alias Flags =
    { windowSize : WindowSize
    , songsJson : Json.Decode.Value
    , visualsJson : Json.Decode.Value
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        deviceInit =
            WindowSize.toDevice flags.windowSize

        -- if the music has an 'Err' then the error gets propagated to the Radio
        radioInit =
            Music.init flags.songsJson
                |> Result.map
                    (\music ->
                        Modules.Radio.init music deviceInit.orientation
                    )

        visualsInit =
            Visual.init flags.visualsJson
                |> Result.map
                    (\visuals ->
                        Modules.Visual.init visuals deviceInit.orientation
                    )
    in
    ( { device = deviceInit
      , radio = radioInit
      , visual = visualsInit
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = WindowResize WindowSize
    | RadioMsg Modules.Radio.Msg
    | VisualMsg Modules.Visual.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize windowSize ->
            ( { model | device = Debug.log "device" <| classifyDevice windowSize }
            , Cmd.none
            )

        RadioMsg radioMsg ->
            case model.radio of
                Err _ ->
                    ( model, Cmd.none )

                Ok radio ->
                    let
                        ( newRadio, newCmd ) =
                            Modules.Radio.update radioMsg radio
                                |> Tuple.mapSecond (Cmd.map RadioMsg)
                    in
                    ( { model | radio = Ok newRadio }
                    , newCmd
                    )

        VisualMsg visualMsg ->
            case model.visual of
                Err _ ->
                    ( model, Cmd.none )

                Ok visual ->
                    let
                        ( newVisual, newCmd ) =
                            Modules.Visual.update visualMsg visual
                                |> Tuple.mapSecond (Cmd.map VisualMsg)
                    in
                    ( { model | visual = Ok newVisual }
                    , newCmd
                    )



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
            [ model.radio
                |> Result.map Modules.Radio.getMusic
                |> Result.map (.currentSong >> .source)
                |> Result.withDefault ""
                |> Attr.src
            , Attr.id "audio-player" -- so the javascript can find it
            ]
            []
        ]



--the actual viewing port


page : Model -> Element Msg
page model =
    case model.device.orientation of
        Landscape ->
            row
                [ width fill
                , height fill
                ]
                [ el
                    [ width <| fillPortion 3
                    , height fill
                    ]
                  <|
                    pictureView model
                , el
                    [ width <| fillPortion 2
                    , height fill
                    ]
                  <|
                    musicView model
                ]

        Portrait ->
            column
                [ width fill
                , height fill
                ]
                [ el
                    [ height <| fillPortion 3
                    , width fill
                    ]
                  <|
                    pictureView model
                , el
                    [ height <| fillPortion 2
                    , width fill
                    ]
                  <|
                    musicView model
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
        case model.visual of
            Ok visual ->
                Modules.Visual.view visual
                    |> Element.map VisualMsg

            Err errors ->
                errorView "Cannot view picture :(" errors


musicView : Model -> Element Msg
musicView model =
    case model.radio of
        Err errors ->
            el
                [ width <| fillPortion 2 ]
            <|
                errorView "Cannot play music :(" errors

        Ok radio ->
            Modules.Radio.view radio
                |> Element.map RadioMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize
            (\x y ->
                WindowSize.fromXY x y
                    |> WindowResize
            )
        , Modules.Radio.subscriptions
            |> Sub.map RadioMsg
        ]
