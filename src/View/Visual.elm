module View.Visual exposing (view)

{-| Configure and display a Visual (picture/video)
-}

import Element exposing (Element, Orientation(..), fill, height, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FontAwesome.Solid
import Html
import Html.Attributes
import Json.Encode
import UiUtils.Colors as Colors
import UiUtils.Icon as Icon
import Visual exposing (Visuals)


type alias Options msg =
    { visualInfo : Visuals
    , orientation : Orientation
    , requestNewVisualMsg : msg
    }


view : Options msg -> Element msg
view options =
    case options.orientation of
        Landscape ->
            Element.el
                [ width <| Element.fillPortion 3
                , height fill
                , Element.padding 40
                ]
                (picture options)

        Portrait ->
            Element.el
                [ width fill
                , height <| Element.fill
                , Element.padding 40
                ]
                (picture options)


picture : Options msg -> Element msg
picture options =
    Element.el
        [ Border.rounded 20
        , Border.solid
        , Border.width 20
        , Border.color Colors.lightBlue
        , width fill
        , height fill
        , Element.pointer
        , Element.mouseDown [ Border.color Colors.light ]
        , Events.onClick options.requestNewVisualMsg
        ]
    <|
        let
            current =
                options.visualInfo.current
        in
        case Visual.getVisualType current.src of
            Ok Visual.Image ->
                Element.el
                    [ Background.image current.src
                    , width fill
                    , height fill
                    ]
                    Element.none

            Ok Visual.Video ->
                Element.el
                    [ width fill
                    , height fill
                    , Element.centerX
                    , Element.centerY
                    ]
                <|
                    displayVideo current.src

            Err errors ->
                Element.el
                    [ width fill
                    , height fill
                    ]
                <|
                    Element.paragraph
                        []
                        [ Element.text current.id
                        , Element.text " is in an unrecognized format! We cannot play this file."
                        , Element.text (Debug.toString errors)
                        ]



-- lmao we need Json encoding for html properties? Wack stuff


displayVideo : String -> Element msg
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
