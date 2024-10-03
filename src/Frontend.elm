module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Lamdera
import Palette.Color as Color
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
      , message = "Welcome to Cooperative Sudoku!"
      , sudokuGrid = List.repeat 9 (List.repeat 9 (Changeable 0))
      , userGrid = List.repeat 9 (List.repeat 9 (Changeable 0))
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


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NewSudokuGridToFrontend grid ->
            ( { model | sudokuGrid = grid, userGrid = grid }, Cmd.none )

        UpdatedUserGridToFrontend grid ->
            ( { model | userGrid = grid }, Cmd.none )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Cooperative Sudoku"
    , body =
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "align-items" "center"
            , Attr.style "font-family" "Arial, sans-serif"
            , Attr.style "padding" "20px"
            , Attr.style "max-width" "100vw"
            , Attr.style "box-sizing" "border-box"
            ]
            [ h1
                [ Attr.style "color" "#333"
                , Attr.style "font-size" "clamp(24px, 5vw, 36px)"
                , Attr.style "text-align" "center"
                ]
                [ text "Cooperative Sudoku" ]
            , viewSudokuGrid model.sudokuGrid model.userGrid model.selectedCell
            , viewDigitButtons
            ]
        ]
    }


viewSudokuGrid : SudokuGrid -> SudokuGrid -> Maybe ( Int, Int ) -> Html FrontendMsg
viewSudokuGrid originalGrid userGrid selectedCell =
    div
        [ Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(9, 1fr)"
        , Attr.style "width" "min(90vw, 450px)"
        , Attr.style "height" "min(90vw, 450px)"
        , Attr.style "border" "2px solid #333"
        , Attr.style "margin" "0 auto"
        ]
        (List.concat (List.indexedMap (viewSudokuRow originalGrid userGrid selectedCell) userGrid))


viewSudokuRow : SudokuGrid -> SudokuGrid -> Maybe ( Int, Int ) -> Int -> List DigitValue -> List (Html FrontendMsg)
viewSudokuRow originalGrid userGrid selectedCell rowIndex row =
    List.indexedMap (viewSudokuCell originalGrid userGrid selectedCell rowIndex) row


viewSudokuCell : SudokuGrid -> SudokuGrid -> Maybe ( Int, Int ) -> Int -> Int -> DigitValue -> Html FrontendMsg
viewSudokuCell originalGrid userGrid selectedCell rowIndex colIndex value =
    let
        isOriginal =
            case getCell rowIndex colIndex originalGrid of
                Just (NotChangeable _) ->
                    True

                Just (Changeable _) ->
                    False

                Nothing ->
                    True

        isCompleted =
            isRowCompleted rowIndex userGrid
                || isColumnCompleted colIndex userGrid
                || isSquareCompleted rowIndex colIndex userGrid

        isSelected =
            selectedCell == Just ( rowIndex, colIndex )

        backgroundColor =
            if isSelected then
                "#007bff"
                -- Dark blue for selected cell

            else if isCompleted then
                Color.toHex Color.Completed

            else if isOriginal then
                Color.toHex Color.Original

            else
                case value of
                    NotChangeable _ ->
                        Color.toHex Color.Input

                    Changeable 0 ->
                        Color.toHex Color.Background

                    Changeable _ ->
                        Color.toHex Color.Input

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

        cellValue =
            case value of
                NotChangeable n ->
                    n

                Changeable n ->
                    n
    in
    div
        [ Attr.style "width" "100%"
        , Attr.style "height" "100%"
        , Attr.style "display" "flex"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        , Attr.style "font-size" "clamp(16px, 4vw, 24px)"
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
        , Attr.style "color"
            (if isSelected then
                "white"

             else
                "black"
            )
        , onClick (SelectCell rowIndex colIndex)
        ]
        [ text
            (if cellValue == 0 then
                ""

             else
                String.fromInt cellValue
            )
        ]


viewDigitButtons : Html FrontendMsg
viewDigitButtons =
    div
        [ Attr.style "display" "flex"
        , Attr.style "justify-content" "center"
        , Attr.style "margin-top" "20px"
        ]
        (List.range 1 9
            |> List.map
                (\digit ->
                    button
                        [ onClick (InputDigit digit)
                        , Attr.style "margin" "0 5px"
                        , Attr.style "padding" "10px 15px"
                        , Attr.style "font-size" "18px"
                        ]
                        [ text (String.fromInt digit) ]
                )
        )


isRowCompleted : Int -> SudokuGrid -> Bool
isRowCompleted rowIndex grid =
    grid
        |> List.drop rowIndex
        |> List.head
        |> Maybe.withDefault []
        |> List.map
            (\digit ->
                case digit of
                    NotChangeable n ->
                        n

                    Changeable n ->
                        n
            )
        |> List.sort
        |> (==) (List.range 1 9)


isColumnCompleted : Int -> SudokuGrid -> Bool
isColumnCompleted colIndex grid =
    grid
        |> List.map
            (\row ->
                List.drop colIndex row
                    |> List.head
                    |> Maybe.withDefault (Changeable 0)
                    |> (\digit ->
                            case digit of
                                NotChangeable n ->
                                    n

                                Changeable n ->
                                    n
                       )
            )
        |> List.sort
        |> (==) (List.range 1 9)


isSquareCompleted : Int -> Int -> SudokuGrid -> Bool
isSquareCompleted rowIndex colIndex grid =
    let
        squareStartRow =
            rowIndex // 3 * 3

        squareStartCol =
            colIndex // 3 * 3

        squareValues =
            List.range 0 2
                |> List.concatMap
                    (\r ->
                        List.range 0 2
                            |> List.filterMap
                                (\c ->
                                    getCell (squareStartRow + r) (squareStartCol + c) grid
                                )
                    )
                |> List.map
                    (\digit ->
                        case digit of
                            NotChangeable n ->
                                n

                            Changeable n ->
                                n
                    )
    in
    List.sort squareValues == List.range 1 9


getCell : Int -> Int -> SudokuGrid -> Maybe DigitValue
getCell row col grid =
    grid
        |> List.drop row
        |> List.head
        |> Maybe.andThen (List.drop col >> List.head)


updateGrid : Int -> Int -> Int -> SudokuGrid -> SudokuGrid
updateGrid row col value grid =
    List.indexedMap
        (\r rowList ->
            if r == row then
                List.indexedMap
                    (\c cellValue ->
                        if c == col then
                            Changeable value

                        else
                            cellValue
                    )
                    rowList

            else
                rowList
        )
        grid
