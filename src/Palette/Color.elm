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
    | BorderLight
    | BorderDark
    | ButtonBackground
    | LoadingSpinner
    | LoadingSpinnerBorder
    | SameNumber -- Add this new color


toHex : Color -> String
toHex color =
    case color of
        Primary ->
            "#007bff"

        Secondary ->
            "#6c757d"

        Background ->
            "#f0f4f8"

        Text ->
            "#333333"

        Completed ->
            "#a5d6a7"

        Original ->
            "#f0f0f0"

        Input ->
            "#ffffff"

        Selected ->
            "#4a90e2"

        BorderLight ->
            "#d0d0d0"

        BorderDark ->
            "#2c3e50"

        ButtonBackground ->
            "#e0e0e0"

        LoadingSpinner ->
            "#3498db"

        LoadingSpinnerBorder ->
            "#f3f3f3"

        SameNumber ->
            "#e6f3ff"



-- Light blue color for highlighting same numbers
