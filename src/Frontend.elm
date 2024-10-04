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


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NewSudokuGridToFrontend grid ->
            ( { model | grid = Just grid, selectedCell = Nothing }, Cmd.none )

        UpdatedUserGridToFrontend grid ->
            ( { model | grid = Just grid }, Cmd.none )


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
            , case model.grid of
                Just grid ->
                    viewSudokuGrid grid model.selectedCell

                Nothing ->
                    text "Loading Sudoku grid..."
            , viewDigitButtons
            ]
        ]
    }


viewSudokuGrid : SudokuGridFrontend -> Maybe ( Int, Int ) -> Html FrontendMsg
viewSudokuGrid grid selectedCell =
    div
        [ Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(9, 1fr)"
        , Attr.style "width" "min(90vw, 450px)"
        , Attr.style "height" "min(90vw, 450px)"
        , Attr.style "border" "2px solid #333"
        , Attr.style "margin" "0 auto"
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
            isRowCompleted rowIndex grid
                || isColumnCompleted colIndex grid
                || isSquareCompleted rowIndex colIndex grid

        isSelected =
            selectedCell == Just ( rowIndex, colIndex )

        backgroundColor =
            if isSelected then
                Color.toHex Color.Selected

            else if isCompleted then
                Color.toHex Color.Completed

            else if isOriginal then
                Color.toHex Color.Original

            else
                case cellState of
                    NotChangeable _ ->
                        Color.toHex Color.Input

                    Changeable _ ->
                        Color.toHex Color.Input

                    NoValue ->
                        Color.toHex Color.Background

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
                [ onClick (SelectCell rowIndex colIndex) ]
    in
    div
        ([ Attr.style "width" "100%"
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
         , Attr.style "cursor"
            (if isOriginal then
                "not-allowed"

             else
                "pointer"
            )
         ]
            ++ cellClickHandler
        )
        [ text cellValue ]


viewDigitButtons : Html FrontendMsg
viewDigitButtons =
    div
        [ Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(3, 1fr)"
        , Attr.style "gap" "10px"
        , Attr.style "width" "min(90vw, 300px)"
        , Attr.style "margin" "20px auto 0"
        ]
        (List.range 1 9
            |> List.map
                (\digit ->
                    button
                        [ onClick (InputDigit digit)
                        , Attr.style "padding" "10px"
                        , Attr.style "font-size" "clamp(16px, 4vw, 24px)"
                        , Attr.style "width" "100%"
                        , Attr.style "aspect-ratio" "1 / 1"
                        , Attr.style "border" "1px solid #ccc"
                        , Attr.style "background-color" "#f0f0f0"
                        , Attr.style "cursor" "pointer"
                        ]
                        [ text (String.fromInt digit) ]
                )
        )


isRowCompleted : Int -> SudokuGridFrontend -> Bool
isRowCompleted rowIndex grid =
    grid
        |> List.drop rowIndex
        |> List.head
        |> Maybe.withDefault []
        |> List.map
            (\cellState ->
                case cellState of
                    NotChangeable n ->
                        n

                    Changeable n ->
                        n

                    NoValue ->
                        0
            )
        |> List.sort
        |> (==) (List.range 1 9)


isColumnCompleted : Int -> SudokuGridFrontend -> Bool
isColumnCompleted colIndex grid =
    grid
        |> List.map
            (\row ->
                List.drop colIndex row
                    |> List.head
                    |> Maybe.withDefault NoValue
                    |> (\cellState ->
                            case cellState of
                                NotChangeable n ->
                                    n

                                Changeable n ->
                                    n

                                NoValue ->
                                    0
                       )
            )
        |> List.sort
        |> (==) (List.range 1 9)


isSquareCompleted : Int -> Int -> SudokuGridFrontend -> Bool
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
                    (\cellState ->
                        case cellState of
                            NotChangeable n ->
                                n

                            Changeable n ->
                                n

                            NoValue ->
                                0
                    )
    in
    List.sort squareValues == List.range 1 9


getCell : Int -> Int -> SudokuGridFrontend -> Maybe CellStateFrontend
getCell row col grid =
    grid
        |> List.drop row
        |> List.head
        |> Maybe.andThen (List.drop col >> List.head)
