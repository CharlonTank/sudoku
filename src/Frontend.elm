module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onMouseDown)
import Json.Decode as Json
import Lamdera
import SudokuLogic
import Types exposing (..)
import Url


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , grid = Nothing
      , selectedCell = Nothing
      }
    , Cmd.none
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        SelectCell row col ->
            ( { model | selectedCell = Just ( row, col ) }, Cmd.none )

        InputDigit digit ->
            case model.selectedCell of
                Just ( row, col ) ->
                    ( model
                    , Lamdera.sendToBackend (UpdateCell row col digit)
                    )

                Nothing ->
                    ( model, Cmd.none )

        RemoveGuess ->
            case model.selectedCell of
                Just ( row, col ) ->
                    ( model
                    , Lamdera.sendToBackend (RemoveCellValue row col)
                    )

                Nothing ->
                    ( model, Cmd.none )

        KeyPressed key ->
            case ( model.selectedCell, key ) of
                ( Just ( row, col ), "Backspace" ) ->
                    ( model
                    , Lamdera.sendToBackend (RemoveCellValue row col)
                    )

                ( Just ( row, col ), digit ) ->
                    case String.toInt digit of
                        Just n ->
                            if n >= 1 && n <= 9 then
                                ( model
                                , Lamdera.sendToBackend (UpdateCell row col n)
                                )

                            else
                                ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NewSudokuGridToFrontend grid ->
            ( { model | grid = Just grid, selectedCell = Nothing }, Cmd.none )

        UpdatedUserGridToFrontend grid ->
            ( { model | grid = Just grid }, Cmd.none )


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
    case model.selectedCell of
        Just _ ->
            Browser.Events.onKeyDown (Json.map KeyPressed (Json.field "key" Json.string))

        Nothing ->
            Sub.none


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Cooperative Sudoku"
    , body =
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "flex-start"
            , Attr.style "height" "100vh"
            , Attr.style "width" "100vw"
            , Attr.style "background" "#f0f4f8"
            , Attr.style "font-family" "Arial, sans-serif"
            , Attr.style "overflow" "hidden"
            , Attr.style "position" "fixed"
            , Attr.style "top" "0"
            , Attr.style "left" "0"
            , Attr.style "padding" "20px"
            , Attr.style "box-sizing" "border-box"
            ]
            [ h1
                [ Attr.style "color" "#333"
                , Attr.style "font-size" "24px"
                , Attr.style "text-align" "center"
                , Attr.style "margin-bottom" "20px"
                , Attr.style "width" "100%"
                ]
                [ text "Cooperative Sudoku" ]
            , div
                [ Attr.style "background-color" "#ffffff"
                , Attr.style "border-radius" "12px"
                , Attr.style "box-shadow" "0 4px 6px rgba(0, 0, 0, 0.1)"
                , Attr.style "padding" "20px"
                , Attr.style "max-width" "95vw"
                , Attr.style "width" "min(95vw, 500px)"
                , Attr.style "max-height" "calc(95vh - 60px)"
                , Attr.style "overflow-y" "auto"
                , Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                ]
                [ case model.grid of
                    Just grid ->
                        viewSudokuGrid grid model.selectedCell

                    Nothing ->
                        viewLoadingSpinner
                , viewDigitButtons
                ]
            ]
        ]
    }


viewLoadingSpinner : Html FrontendMsg
viewLoadingSpinner =
    div
        [ Attr.style "display" "flex"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        , Attr.style "height" "450px"
        ]
        [ div
            [ Attr.style "border" "4px solid #f3f3f3"
            , Attr.style "border-top" "4px solid #3498db"
            , Attr.style "border-radius" "50%"
            , Attr.style "width" "50px"
            , Attr.style "height" "50px"
            , Attr.style "animation" "spin 1s linear infinite"
            ]
            []
        ]


viewSudokuGrid : SudokuGridFrontend -> Maybe ( Int, Int ) -> Html FrontendMsg
viewSudokuGrid grid selectedCell =
    div
        [ Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(9, 1fr)"
        , Attr.style "gap" "1px"
        , Attr.style "width" "100%"
        , Attr.style "aspect-ratio" "1 / 1"
        , Attr.style "border" "2px solid #333"
        , Attr.style "margin" "0 auto 20px"
        , Attr.style "background-color" "#333"
        ]
        (List.concat (List.indexedMap (viewSudokuRow grid selectedCell) grid))


viewSudokuRow : SudokuGridFrontend -> Maybe ( Int, Int ) -> Int -> List CellStateFrontend -> List (Html FrontendMsg)
viewSudokuRow grid selectedCell rowIndex row =
    List.indexedMap (viewSudokuCell grid selectedCell rowIndex) row


viewSudokuCell : SudokuGridFrontend -> Maybe ( Int, Int ) -> Int -> Int -> CellStateFrontend -> Html FrontendMsg
viewSudokuCell grid selectedCell rowIndex colIndex cellState =
    let
        isOriginal =
            case cellState of
                NotChangeable _ ->
                    True

                _ ->
                    False

        isCompleted =
            SudokuLogic.isRowCompleted rowIndex grid
                || SudokuLogic.isColumnCompleted colIndex grid
                || SudokuLogic.isSquareCompleted rowIndex colIndex grid

        isSelected =
            selectedCell == Just ( rowIndex, colIndex )

        backgroundColor =
            if isSelected then
                "#4a90e2"

            else if isCompleted then
                "#a5d6a7"

            else if isOriginal then
                "#f0f0f0"

            else
                "#ffffff"

        textColor =
            if isSelected then
                "#ffffff"

            else if isOriginal then
                "#333333"

            else
                "#666666"

        borderStyle =
            "1px solid #7f8c8d"

        thickBorderStyle =
            "2px solid #2c3e50"

        borderRight =
            if modBy 3 (colIndex + 1) == 0 then
                thickBorderStyle

            else
                borderStyle

        borderBottom =
            if modBy 3 (rowIndex + 1) == 0 then
                thickBorderStyle

            else
                borderStyle

        borderTop =
            if rowIndex == 0 || modBy 3 rowIndex == 0 then
                thickBorderStyle

            else
                borderStyle

        borderLeft =
            if colIndex == 0 || modBy 3 colIndex == 0 then
                thickBorderStyle

            else
                borderStyle

        cellValue =
            case cellState of
                NotChangeable n ->
                    String.fromInt n

                Changeable n ->
                    String.fromInt n

                NoValue ->
                    ""

        cellClickHandler =
            if isOriginal then
                []

            else
                [ onMouseDown (SelectCell rowIndex colIndex) ]
    in
    div
        ([ Attr.style "width" "100%"
         , Attr.style "height" "100%"
         , Attr.style "display" "flex"
         , Attr.style "justify-content" "center"
         , Attr.style "align-items" "center"
         , Attr.style "font-size" "clamp(16px, 5vw, 24px)"
         , Attr.style "font-weight"
            (if isOriginal then
                "bold"

             else
                "normal"
            )
         , Attr.style "background-color" backgroundColor
         , Attr.style "color" textColor
         , Attr.style "cursor"
            (if isOriginal then
                "default"

             else
                "pointer"
            )
         , Attr.style "user-select" "none"
         ]
            ++ cellClickHandler
        )
        [ text cellValue ]


viewDigitButtons : Html FrontendMsg
viewDigitButtons =
    div
        [ Attr.style "display" "flex"
        , Attr.style "justify-content" "space-between"
        , Attr.style "width" "100%"
        , Attr.style "margin-top" "20px"
        ]
        ((List.range 1 9
            |> List.map
                (\digit ->
                    button
                        [ onClick (InputDigit digit)
                        , Attr.style "flex" "1"
                        , Attr.style "padding" "10px 0"
                        , Attr.style "font-size" "16px"
                        , Attr.style "border" "none"
                        , Attr.style "background-color" "#e0e0e0"
                        , Attr.style "color" "#333"
                        , Attr.style "cursor" "pointer"
                        , Attr.style "margin" "0 2px"
                        , Attr.style "border-radius" "4px"
                        ]
                        [ text (String.fromInt digit) ]
                )
         )
            ++ [ button
                    [ onClick RemoveGuess
                    , Attr.style "flex" "1"
                    , Attr.style "padding" "10px 0"
                    , Attr.style "font-size" "16px"
                    , Attr.style "border" "none"
                    , Attr.style "background-color" "#e0e0e0"
                    , Attr.style "color" "#333"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "margin" "0 2px"
                    , Attr.style "border-radius" "4px"
                    ]
                    [ text "X" ]
               ]
        )
