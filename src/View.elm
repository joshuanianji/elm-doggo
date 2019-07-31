module View exposing (view)

import Element exposing (Element, Orientation(..))
import Element.Background as Background
import Element.Font as Font
import FontAwesome.Styles
import Html exposing (Html)
import Html.Attributes as Attr
import Types exposing (Model, Msg(..))
import UiUtils.Colors as Colors
import View.Landscape as Landscape
import View.Portrait as Portrait



-- we have to inject the font awesome stylesheet into our stuff


view : Model -> Element Msg
view model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Font.color Colors.lightBlue
        , Background.color Colors.dark
        ]
        [ FontAwesome.Styles.css |> Element.html
        , audio model |> Element.html
        , page model
        ]



-- hidden element that play music lol
-- Attr.src has the hardest time because it's a nested Maybe in a Result lmao


audio : Model -> Html Msg
audio model =
    Html.div [ Attr.class "elm-audio-player" ]
        [ Html.audio
            [ Attr.src
                (model.music
                    |> Result.toMaybe
                    |> Maybe.andThen .currentSong
                    |> Maybe.map .source
                    |> Maybe.withDefault ""
                )
            , Attr.id "audio-player" -- so the javascript can find it
            ]
            []
        ]


page : Model -> Element Msg
page model =
    case model.device.orientation of
        Landscape ->
            Landscape.view model

        Portrait ->
            Portrait.view model
