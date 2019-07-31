module View.Modules exposing (Align(..), goBack, goForward, nowPlaying, picture, skipButtons, songDescription, songName)

{-| There are key similarities between the view functions of the Portrait and Landscape,
such as the design of the Radio and the picture.
-}

import Convertor
import Element exposing (Element, centerX, centerY, fill, height, pointer, row, shrink, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FontAwesome.Layering as Layering
import FontAwesome.Solid
import Html
import Html.Attributes
import Json.Encode
import Music exposing (Music)
import Picture exposing (PicType(..), Pictures)
import Types exposing (Model, Msg(..))
import UiUtils.Colors as Colors
import UiUtils.Icon as Icon exposing (Icon)



-- PICTURE --


picture : Pictures -> Element Msg
picture picData =
    Element.el
        [ Border.rounded 20
        , Border.solid
        , Border.width 22
        , Border.color Colors.lightBlue
        , width fill
        , height fill
        , pointer
        , Element.mouseDown [ Border.color Colors.light ]
        , Events.onClick ChangePicture
        ]
    <|
        case picData.currentPic of
            Just pic ->
                case Convertor.getPicType pic.src of
                    Ok Image ->
                        Element.el
                            [ Background.image pic.src
                            , width fill
                            , height fill
                            ]
                            Element.none

                    Ok Video ->
                        Element.el
                            [ width fill
                            , height fill
                            , centerX
                            , centerY
                            ]
                        <|
                            displayVideo pic.src

                    Err errors ->
                        Element.el
                            [ width fill
                            , height fill
                            ]
                        <|
                            Element.paragraph
                                []
                                [ Element.text pic.id
                                , Element.text " is in an unrecognized format! We cannot play this file."
                                , Element.text (Debug.toString errors)
                                ]

            Nothing ->
                Element.el
                    [ centerX
                    , centerY
                    , width fill
                    , height fill
                    ]
                    (Element.text "Loading....")



-- lmao we need Json encoding for html properties? Wack stuff


displayVideo : String -> Element Msg
displayVideo url =
    Element.html <|
        Html.video
            [ Html.Attributes.autoplay True
            , Html.Attributes.controls False
            , Html.Attributes.property "muted" (Json.Encode.bool True)
            , Html.Attributes.style "max-height" "100%"
            , Html.Attributes.style "max-width" "100%"
            ]
            [ Html.source
                [ Html.Attributes.src url
                , Html.Attributes.type_ "video/mp4"
                , Html.Attributes.style "max-height" "100%"
                , Html.Attributes.style "max-width" "100%"
                ]
                []
            ]



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


skipButtons : Music -> Element Msg
skipButtons music =
    row
        [ width fill
        , height fill
        , centerX
        ]
        [ goBack music
        , goForward music
        ]


goBack : Music -> Element Msg
goBack music =
    if List.length music.previousSongs == 0 then
        Element.el
            [ width fill, height fill ]
            Element.none

    else
        buttonWrapper GetPreviousSong (Icon.view FontAwesome.Solid.angleDoubleLeft)


goForward : Music -> Element Msg
goForward music =
    if List.length music.nextSongs == 0 then
        buttonWrapper GetNewSong (Icon.view FontAwesome.Solid.angleDoubleRight)

    else
        buttonWrapper GetNewSong
            (Icon.withLayer
                { string = String.fromInt <| List.length music.nextSongs
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
            [ centerX
            , centerY
            , Font.size 150
            , pointer
            , Element.mouseDown [ Font.color Colors.light ]
            , Events.onClick msg
            ]
            icon


type Align
    = Left
    | Right
