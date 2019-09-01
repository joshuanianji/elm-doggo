module UiUtils.Icon exposing (Icon, Options, view, withLayer)

import Element exposing (Color, Element, html)
import FontAwesome.Attributes as Attr
import FontAwesome.Icon as Icon
import FontAwesome.Layering as Layering
import FontAwesome.Solid as Solid
import Html exposing (Html)
import Html.Attributes
import Svg


type alias Icon =
    Icon.Icon


view : Icon -> Element msg
view icon =
    Icon.viewIcon icon
        |> html


type alias Options =
    { string : String
    , textColor : Color
    , mainIcon : Icon
    }



-- with a text layer. Circle behind text layer.


withLayer : Options -> Element msg
withLayer options =
    Layering.layers
        []
        [ textOverlay options options.string
        , Icon.viewIcon options.mainIcon
        ]
        |> html



-- helper function for withLayer


textOverlay : Options -> String -> Html msg
textOverlay options string =
    Layering.layers
        [ Layering.layersTopRight ]
        [ Layering.layers
            []
            [ Icon.viewIcon Solid.circle ]
        , Layering.text
            [ Attr.xs
            , fromColor options.textColor
            , Html.Attributes.style "font-size" "0.5em"
            ]
            string
        ]



-- Html.Attributes.Style wants us to do styles with strings rip.


fromColor : Color -> Html.Attribute msg
fromColor color =
    let
        rgb =
            Element.toRgb color
    in
    Html.Attributes.style
        "color"
        ("rgb("
            ++ (String.fromInt <| round <| (rgb.red * 256 - 1))
            ++ ","
            ++ (String.fromInt <| round <| (rgb.green * 256 - 1))
            ++ ","
            ++ (String.fromInt <| round <| (rgb.blue * 256 - 1))
            ++ ")"
        )
