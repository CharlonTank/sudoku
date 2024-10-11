module Ui.Spinner exposing (view)

import Html exposing (Html, div)
import Html.Attributes as Attr
import Palette.Color as Color


view : Html msg
view =
    div
        [ Attr.style "display" "flex"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        , Attr.style "height" "450px"
        ]
        [ div
            [ Attr.style "border" ("4px solid " ++ Color.toHex Color.LoadingSpinnerBorder)
            , Attr.style "border-top" ("4px solid " ++ Color.toHex Color.LoadingSpinner)
            , Attr.style "border-radius" "50%"
            , Attr.style "width" "50px"
            , Attr.style "height" "50px"
            , Attr.style "animation" "spin 1s linear infinite"
            ]
            []
        ]
