module Ui.Input exposing (view)

import Html exposing (Html, input)
import Html.Attributes as Attr
import Html.Events exposing (onInput)


view : { value : String, placeholder : String, onInput : String -> msg } -> Html msg
view config =
    input
        [ Attr.type_ "text"
        , Attr.value config.value
        , Attr.placeholder config.placeholder
        , Attr.style "margin-bottom" "10px"
        , Attr.style "padding" "5px"
        , Attr.style "width" "200px"
        , onInput config.onInput
        ]
        []
