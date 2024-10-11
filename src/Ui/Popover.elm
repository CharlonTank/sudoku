module Ui.Popover exposing (view)

import Html exposing (Html, div)
import Html.Attributes as Attr
import Palette.Color as Color


view : Bool -> List (Html msg) -> Html msg
view isVisible content =
    if isVisible then
        div
            [ Attr.style "position" "fixed"
            , Attr.style "top" "0"
            , Attr.style "left" "0"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "background-color" "rgba(0, 0, 0, 0.5)"
            , Attr.style "display" "flex"
            , Attr.style "justify-content" "center"
            , Attr.style "align-items" "center"
            , Attr.style "z-index" "1000"
            ]
            [ div
                [ Attr.style "background-color" (Color.toHex Color.Input)
                , Attr.style "padding" "20px"
                , Attr.style "border-radius" "8px"
                , Attr.style "text-align" "center"
                ]
                content
            ]

    else
        Html.text ""
