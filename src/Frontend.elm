module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onMouseDown)
import Json.Decode as Json
import Lamdera exposing (SessionId)
import List.Extra
import Palette.Color as Color
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
      , connectedPlayers = []
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
            let
                position : Position
                position =
                    ( row, col )
            in
            ( { model | selectedCell = Just position }
            , Cmd.none
            )

        InputDigit digit ->
            case model.selectedCell of
                Just position ->
                    ( model
                    , Lamdera.sendToBackend (UpdateCell position digit)
                    )

                Nothing ->
                    ( model, Cmd.none )

        RemoveGuess ->
            case model.selectedCell of
                Just position ->
                    ( model
                    , Lamdera.sendToBackend (RemoveCellValue position)
                    )

                Nothing ->
                    ( model, Cmd.none )

        KeyPressed key ->
            case model.selectedCell of
                Just position ->
                    case key of
                        "Backspace" ->
                            ( model
                            , Lamdera.sendToBackend (RemoveCellValue position)
                            )

                        digit ->
                            case String.toInt digit of
                                Just n ->
                                    if n >= 1 && n <= 9 then
                                        ( model
                                        , Lamdera.sendToBackend (UpdateCell position n)
                                        )

                                    else
                                        ( model, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NewSudokuGridToFrontend grid ->
            ( { model | grid = Just grid, selectedCell = Nothing }, Cmd.none )

        UpdatedUserGridToFrontend grid ->
            ( { model | grid = Just grid }, Cmd.none )

        ConnectedPlayersChanged players ->
            ( { model | connectedPlayers = players }, Cmd.none )


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
            , Attr.style "background" (Color.toHex Color.Background)
            , Attr.style "font-family" "Arial, sans-serif"
            , Attr.style "overflow" "hidden"
            , Attr.style "position" "fixed"
            , Attr.style "top" "0"
            , Attr.style "left" "0"
            , Attr.style "padding" "20px"
            , Attr.style "box-sizing" "border-box"
            ]
            [ h1
                [ Attr.style "color" (Color.toHex Color.Text)
                , Attr.style "font-size" "24px"
                , Attr.style "text-align" "center"
                , Attr.style "margin-bottom" "20px"
                , Attr.style "width" "100%"
                ]
                [ text "Cooperative Sudoku" ]
            , div
                [ Attr.style "background-color" (Color.toHex Color.Input)
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
            , viewConnectedPlayers model.connectedPlayers
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
            [ Attr.style "border" ("4px solid " ++ Color.toHex Color.LoadingSpinnerBorder)
            , Attr.style "border-top" ("4px solid " ++ Color.toHex Color.LoadingSpinner)
            , Attr.style "border-radius" "50%"
            , Attr.style "width" "50px"
            , Attr.style "height" "50px"
            , Attr.style "animation" "spin 1s linear infinite"
            ]
            []
        ]


viewSudokuGrid : SudokuGridFrontend -> Maybe Position -> Html FrontendMsg
viewSudokuGrid grid selectedCell =
    div
        [ Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(9, 1fr)"
        , Attr.style "gap" "0" -- Changed from "1px" to "0"
        , Attr.style "width" "100%"
        , Attr.style "aspect-ratio" "1 / 1"
        , Attr.style "margin" "0 auto 20px"

        -- Removed the border style from here
        ]
        (List.concat (List.indexedMap (viewSudokuRow grid selectedCell) grid))


viewSudokuRow : SudokuGridFrontend -> Maybe Position -> Int -> List CellStateFrontend -> List (Html FrontendMsg)
viewSudokuRow grid selectedCell rowIndex row =
    List.indexedMap
        (\colIndex cellState ->
            let
                position : Position
                position =
                    ( rowIndex, colIndex )
            in
            viewSudokuCell grid selectedCell position cellState
        )
        row


viewSudokuCell : SudokuGridFrontend -> Maybe Position -> Position -> CellStateFrontend -> Html FrontendMsg
viewSudokuCell grid selectedCell ( rowIndex, colIndex ) cellState =
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

        selectedNumber =
            selectedCell
                |> Maybe.andThen
                    (\( r, c ) ->
                        grid
                            |> List.Extra.getAt r
                            |> Maybe.andThen (List.Extra.getAt c)
                            |> Maybe.andThen
                                (\state ->
                                    case state of
                                        NotChangeable n ->
                                            Just n

                                        Changeable n ->
                                            Just n

                                        NoValue ->
                                            Nothing
                                )
                    )

        cellNumber =
            case cellState of
                NotChangeable n ->
                    Just n

                Changeable n ->
                    Just n

                NoValue ->
                    Nothing

        isSameNumber =
            selectedNumber
                |> Maybe.map (\n -> cellNumber == Just n)
                |> Maybe.withDefault False

        backgroundColor =
            if isSelected then
                Color.toHex Color.Selected

            else if isSameNumber then
                Color.toHex Color.SameNumber

            else if isCompleted then
                Color.toHex Color.Completed

            else if isOriginal then
                Color.toHex Color.Original

            else
                Color.toHex Color.Input

        textColor =
            if isSelected then
                Color.toHex Color.Input

            else if isOriginal then
                Color.toHex Color.Text

            else
                Color.toHex Color.Secondary

        simpleBorderStyle =
            "1px solid " ++ Color.toHex Color.BorderLight

        thickBorderStyle =
            "2px solid " ++ Color.toHex Color.BorderDark

        borderRight =
            if modBy 3 (colIndex + 1) == 0 then
                thickBorderStyle

            else
                simpleBorderStyle

        borderBottom =
            if modBy 3 (rowIndex + 1) == 0 then
                thickBorderStyle

            else
                simpleBorderStyle

        borderTop =
            if rowIndex == 0 then
                thickBorderStyle

            else if modBy 3 rowIndex == 0 then
                thickBorderStyle

            else
                simpleBorderStyle

        borderLeft =
            if colIndex == 0 then
                thickBorderStyle

            else if modBy 3 colIndex == 0 then
                thickBorderStyle

            else
                simpleBorderStyle

        cellValue =
            case cellState of
                NotChangeable n ->
                    String.fromInt n

                Changeable n ->
                    String.fromInt n

                NoValue ->
                    ""

        cellClickHandler =
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
         , Attr.style "cursor" "pointer"
         , Attr.style "user-select" "none"
         , Attr.style "border-right" borderRight
         , Attr.style "border-bottom" borderBottom
         , Attr.style "border-top" borderTop
         , Attr.style "border-left" borderLeft
         , Attr.style "box-sizing" "border-box"
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
                        , Attr.style "background-color" (Color.toHex Color.ButtonBackground)
                        , Attr.style "color" (Color.toHex Color.Text)
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
                    , Attr.style "background-color" (Color.toHex Color.ButtonBackground)
                    , Attr.style "color" (Color.toHex Color.Text)
                    , Attr.style "cursor" "pointer"
                    , Attr.style "margin" "0 2px"
                    , Attr.style "border-radius" "4px"
                    ]
                    [ text "X" ]
               ]
        )


viewConnectedPlayers : List Player -> Html FrontendMsg
viewConnectedPlayers players =
    div
        [ Attr.style "margin-top" "20px"
        , Attr.style "color" (Color.toHex Color.Text)
        , Attr.style "font-size" "14px"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "flex-start"
        ]
        ([ div [ Attr.style "font-weight" "bold", Attr.style "margin-bottom" "5px" ] [ text "Connected players:" ] ]
            ++ List.map viewPlayer players
        )


viewPlayer : Player -> Html FrontendMsg
viewPlayer player =
    div
        [ Attr.style "margin-bottom" "3px"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        ]
        [ text (String.left 4 player.sessionId)
        , viewLifes player.lifes
        ]


viewLifes : Maybe Life -> Html FrontendMsg
viewLifes maybeLife =
    let
        lifeSymbol =
            "❤️"

        lifesString =
            case maybeLife of
                Just ThreeLife ->
                    String.repeat 3 lifeSymbol

                Just TwoLife ->
                    String.repeat 2 lifeSymbol

                Just OneLife ->
                    lifeSymbol

                Nothing ->
                    ""
    in
    span
        [ Attr.style "margin-left" "5px"
        , Attr.style "font-size" "12px"
        ]
        [ text lifesString ]
