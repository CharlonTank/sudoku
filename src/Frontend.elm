module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Lamdera
import Types exposing (..)
import Url


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , message = "Welcome to Sudoku Generator!"
      , sudokuGrid = List.repeat 9 (List.repeat 9 0)
      , userGrid = List.repeat 9 (List.repeat 9 0)
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

        GenerateSudoku ->
            ( { model | userGrid = List.repeat 9 (List.repeat 9 0) }
            , Lamdera.sendToBackend RequestNewSudoku
            )

        UserInput row col value ->
            let
                newValue =
                    value
                        |> String.left 1
                        |> String.toInt
                        |> Maybe.withDefault 0

                newUserGrid =
                    updateGrid row col newValue model.userGrid
            in
            ( { model | userGrid = newUserGrid }, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NewSudokuGridToFrontend grid ->
            ( { model | sudokuGrid = grid, userGrid = grid }, Cmd.none )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Sudoku Generator"
    , body =
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "align-items" "center"
            , Attr.style "font-family" "Arial, sans-serif"
            , Attr.style "padding" "20px"
            ]
            [ h1 [ Attr.style "color" "#333" ] [ text "Sudoku Generator" ]
            , button
                [ onClick GenerateSudoku
                , Attr.style "background-color" "#4CAF50"
                , Attr.style "border" "none"
                , Attr.style "color" "white"
                , Attr.style "padding" "15px 32px"
                , Attr.style "text-align" "center"
                , Attr.style "text-decoration" "none"
                , Attr.style "display" "inline-block"
                , Attr.style "font-size" "16px"
                , Attr.style "margin" "20px 0"
                , Attr.style "cursor" "pointer"
                , Attr.style "border-radius" "4px"
                ]
                [ text "Generate New Sudoku" ]
            , viewSudokuGrid model.sudokuGrid model.userGrid
            ]
        ]
    }


viewSudokuGrid : SudokuGrid -> SudokuGrid -> Html FrontendMsg
viewSudokuGrid originalGrid userGrid =
    div
        [ Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(9, 1fr)"
        , Attr.style "width" "450px"
        , Attr.style "height" "450px"
        , Attr.style "border" "2px solid #333"
        ]
        (List.concat (List.indexedMap (viewSudokuRow originalGrid) userGrid))


viewSudokuRow : SudokuGrid -> Int -> List Int -> List (Html FrontendMsg)
viewSudokuRow originalGrid rowIndex row =
    List.indexedMap (viewSudokuCell originalGrid rowIndex) row


viewSudokuCell : SudokuGrid -> Int -> Int -> Int -> Html FrontendMsg
viewSudokuCell originalGrid rowIndex colIndex value =
    let
        originalValue =
            getCell rowIndex colIndex originalGrid

        isOriginal =
            originalValue /= 0

        backgroundColor =
            if isOriginal then
                "#e0e0e0"

            else if value /= 0 then
                "#f0f0f0"

            else
                "white"

        borderStyle =
            "1px solid #000"

        thickBorderStyle =
            "2px solid #000"

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
    in
    div
        [ Attr.style "width" "50px"
        , Attr.style "height" "50px"
        , Attr.style "display" "flex"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        , Attr.style "font-size" "24px"
        , Attr.style "font-weight"
            (if isOriginal then
                "bold"

             else
                "normal"
            )
        , Attr.style "background-color" backgroundColor
        , Attr.style "border-right" borderRight
        , Attr.style "border-bottom" borderBottom
        , Attr.style "border-top" borderTop
        , Attr.style "border-left" borderLeft
        , Attr.style "box-sizing" "border-box"
        ]
        [ if isOriginal then
            text (String.fromInt originalValue)

          else
            input
                [ Attr.type_ "text"
                , Attr.value
                    (if value == 0 then
                        ""

                     else
                        String.fromInt value
                    )
                , Attr.style "width" "100%"
                , Attr.style "height" "100%"
                , Attr.style "border" "none"
                , Attr.style "text-align" "center"
                , Attr.style "font-size" "24px"
                , Attr.style "background-color" "transparent"
                , Attr.style "outline" "none"
                , Attr.maxlength 1
                , onInput (UserInput rowIndex colIndex)
                ]
                []
        ]



-- Helper functions


getCell : Int -> Int -> SudokuGrid -> Int
getCell row col grid =
    grid
        |> List.drop row
        |> List.head
        |> Maybe.withDefault []
        |> List.drop col
        |> List.head
        |> Maybe.withDefault 0


updateGrid : Int -> Int -> Int -> SudokuGrid -> SudokuGrid
updateGrid row col value grid =
    List.indexedMap
        (\r rowList ->
            if r == row then
                List.indexedMap
                    (\c cellValue ->
                        if c == col then
                            value

                        else
                            cellValue
                    )
                    rowList

            else
                rowList
        )
        grid
