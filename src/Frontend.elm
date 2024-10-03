module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onInput)
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
      , message = "Welcome to Cooperative Sudoku!"
      , sudokuGrid = List.repeat 9 (List.repeat 9 (Changeable 0))
      , userGrid = List.repeat 9 (List.repeat 9 (Changeable 0))
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

        UserInput row col value ->
            let
                newValue =
                    value
                        |> String.left 1
                        |> String.toInt
                        |> Maybe.withDefault 0
            in
            ( model
            , Lamdera.sendToBackend (UpdateCell row col newValue)
            )


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
            , viewSudokuGrid model.sudokuGrid model.userGrid
            ]
        ]
    }


viewSudokuGrid : SudokuGrid -> SudokuGrid -> Html FrontendMsg
viewSudokuGrid originalGrid userGrid =
    div
        [ Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(9, 1fr)"
        , Attr.style "width" "min(90vw, 450px)"
        , Attr.style "height" "min(90vw, 450px)"
        , Attr.style "border" "2px solid #333"
        , Attr.style "margin" "0 auto"
        ]
        (List.concat (List.indexedMap (viewSudokuRow originalGrid userGrid) userGrid))


viewSudokuRow : SudokuGrid -> SudokuGrid -> Int -> List DigitValue -> List (Html FrontendMsg)
viewSudokuRow originalGrid userGrid rowIndex row =
    List.indexedMap (viewSudokuCell originalGrid userGrid rowIndex) row


viewSudokuCell : SudokuGrid -> SudokuGrid -> Int -> Int -> DigitValue -> Html FrontendMsg
viewSudokuCell originalGrid userGrid rowIndex colIndex value =
    let
        originalValue =
            getCell rowIndex colIndex originalGrid

        isOriginal =
            case originalValue of
                NotChangeable _ ->
                    True

                Changeable _ ->
                    False

        isCompleted =
            isRowCompleted rowIndex userGrid
                || isColumnCompleted colIndex userGrid
                || isSquareCompleted rowIndex colIndex userGrid

        backgroundColor =
            if isCompleted then
                "#e6f3ff"
                -- Light blue for completed sections

            else if isOriginal then
                "#e0e0e0"

            else
                case value of
                    NotChangeable _ ->
                        "#f0f0f0"

                    Changeable 0 ->
                        "white"

                    Changeable _ ->
                        "#f0f0f0"

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
        ]
        [ if isOriginal then
            text (String.fromInt cellValue)

          else
            input
                [ Attr.type_ "text"
                , Attr.value
                    (if cellValue == 0 then
                        ""

                     else
                        String.fromInt cellValue
                    )
                , Attr.style "width" "100%"
                , Attr.style "height" "100%"
                , Attr.style "border" "none"
                , Attr.style "text-align" "center"
                , Attr.style "font-size" "inherit"
                , Attr.style "background-color" "transparent"
                , Attr.style "outline" "none"
                , Attr.maxlength 1
                , onInput (UserInput rowIndex colIndex)
                ]
                []
        ]



-- Helper functions for checking completions


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
                            |> List.map
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



-- Helper function to get a cell value from the grid


getCell : Int -> Int -> SudokuGrid -> DigitValue
getCell row col grid =
    grid
        |> List.drop row
        |> List.head
        |> Maybe.withDefault []
        |> List.drop col
        |> List.head
        |> Maybe.withDefault (Changeable 0)



-- Helper function to update a cell in the grid


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
