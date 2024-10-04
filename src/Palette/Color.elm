module Palette.Color exposing (Color(..), toHex)


type Color
    = Primary
    | Secondary
    | Background
    | Text
    | Completed
    | Original
    | Input
    | Selected


toHex : Color -> String
toHex color =
    case color of
        Primary ->
            "#007bff"

        Secondary ->
            "#6c757d"

        Background ->
            "#ffffff"

        Text ->
            "#333333"

        Completed ->
            "#e6f3ff"

        Original ->
            "#e0e0e0"

        Input ->
            "#f0f0f0"

        Selected ->
            "#007bff"
