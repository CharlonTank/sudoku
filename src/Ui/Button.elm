module Ui.Button exposing (view)

import Html exposing (Html, button)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Palette.Color as Color


view : { onClick : msg, label : String } -> Html msg
view config =
    button
        [ onClick config.onClick
        , Attr.style "padding" "10px 20px"
        , Attr.style "font-size" "16px"
        , Attr.style "background-color" (Color.toHex Color.ButtonBackground)
        , Attr.style "color" (Color.toHex Color.Text)
        , Attr.style "border" "none"
        , Attr.style "border-radius" "4px"
        , Attr.style "cursor" "pointer"
        ]
        [ Html.text config.label ]
