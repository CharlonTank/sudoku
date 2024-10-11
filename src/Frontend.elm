module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput, onMouseDown)
import Json.Decode as Json
import Lamdera exposing (SessionId)
import List.Extra
import Palette.Color as Color
import SudokuLogic
import Time
import Types exposing (..)
import Ui.Button
import Ui.Input
import Ui.Popover
import Ui.Spinner
import Url
import Url.Parser as Parser exposing ((</>), Parser, int, map, oneOf, s, string)


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged -- Update this line
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    let
        route =
            Parser.parse routeParser url |> Maybe.withDefault Home
    in
    ( { key = key
      , grid = Nothing
      , selectedCell = Nothing
      , connectedPlayers = []
      , currentPlayer = Nothing
      , gameoverPopoverOn = False
      , namePopoverOn = False
      , nameInput = ""
      , route = route -- Add this line
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
            let
                newRoute =
                    Parser.parse routeParser url |> Maybe.withDefault Home
            in
            ( { model | route = newRoute }, Cmd.none )

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
                    if not model.gameoverPopoverOn && playerCanMakeGuess model.currentPlayer then
                        ( model
                        , Lamdera.sendToBackend (UpdateCell position digit)
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        RemoveGuess ->
            case model.selectedCell of
                Just position ->
                    if not model.gameoverPopoverOn && playerCanMakeGuess model.currentPlayer then
                        ( model
                        , Lamdera.sendToBackend (RemoveCellValue position)
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        KeyPressed key ->
            if not model.gameoverPopoverOn && playerCanMakeGuess model.currentPlayer then
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

            else
                ( model, Cmd.none )

        CloseGameOverPopover ->
            ( { model | gameoverPopoverOn = False }, Cmd.none )

        OpenNamePopover ->
            ( { model | namePopoverOn = True }, Cmd.none )

        CloseNamePopover ->
            ( { model | namePopoverOn = False, nameInput = "" }, Cmd.none )

        UpdateNameInput name ->
            ( { model | nameInput = name }, Cmd.none )

        SaveName ->
            ( { model | namePopoverOn = False, nameInput = "" }
            , Lamdera.sendToBackend (UpdatePlayerName model.nameInput)
            )

        ResetBackend ->
            ( model, Lamdera.sendToBackend ResetBackendRequest )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NewSudokuGridToFrontend grid ->
            ( { model | grid = Just grid, selectedCell = Nothing }, Cmd.none )

        UpdatedUserGridToFrontend grid ->
            ( { model | grid = Just grid }, Cmd.none )

        ConnectedPlayersChanged players ->
            let
                updatedCurrentPlayer =
                    model.currentPlayer
                        |> Maybe.andThen (\cp -> List.Extra.find (\p -> p.sessionId == cp.sessionId) players)

                gameoverPopoverOn =
                    updatedCurrentPlayer
                        |> Maybe.map (\p -> p.lifes == Nothing)
                        |> Maybe.withDefault False
            in
            ( { model
                | connectedPlayers = players
                , currentPlayer = updatedCurrentPlayer
                , gameoverPopoverOn = gameoverPopoverOn
              }
            , Cmd.none
            )

        SetCurrentPlayer player ->
            ( { model | currentPlayer = Just player }, Cmd.none )

        PlayerNameUpdated player ->
            ( { model | currentPlayer = Just player }, Cmd.none )

        BackendResetConfirmation ->
            ( { model | grid = Nothing, selectedCell = Nothing }, Cmd.none )


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ case model.selectedCell of
            Just _ ->
                Browser.Events.onKeyDown (Json.map KeyPressed (Json.field "key" Json.string))

            Nothing ->
                Sub.none
        ]


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Cooperative Sudoku"
    , body =
        [ case model.route of
            Home ->
                viewHome model

            Admin ->
                viewAdmin
        ]
    }


viewHome : FrontendModel -> Html FrontendMsg
viewHome model =
    div
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
        , viewCurrentPlayer model.currentPlayer
        , viewNamePopover model.namePopoverOn model.nameInput
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
                    div []
                        [ viewSudokuGrid grid model.selectedCell
                        , viewDigitButtons grid
                        ]

                Nothing ->
                    viewLoadingSpinner
            ]
        , model.currentPlayer
            |> Maybe.map
                (\currentPlayer ->
                    viewConnectedPlayers
                        (model.connectedPlayers
                            |> List.filter (\p -> p.sessionId /= currentPlayer.sessionId)
                        )
                )
            |> Maybe.withDefault (text "")
        , viewGameOverPopover model.gameoverPopoverOn
        ]


viewAdmin : Html FrontendMsg
viewAdmin =
    div []
        [ h1 [] [ text "Admin Page" ]
        , Ui.Button.view
            { onClick = ResetBackend
            , label = "RESET BACKEND"
            }
        ]


viewLoadingSpinner : Html FrontendMsg
viewLoadingSpinner =
    Ui.Spinner.view


viewSudokuGrid : SudokuGridFrontend -> Maybe Position -> Html FrontendMsg
viewSudokuGrid grid selectedCell =
    div
        [ Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(9, 1fr)"
        , Attr.style "gap" "0"
        , Attr.style "width" "100%"
        , Attr.style "aspect-ratio" "1 / 1"
        , Attr.style "margin" "0 auto 20px"
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

                                        WrongGuess n ->
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

                WrongGuess n ->
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
            case cellState of
                WrongGuess _ ->
                    Color.toHex Color.WrongGuess

                _ ->
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

                WrongGuess n ->
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
         , Attr.style "font-size" "clamp(20px, 6vw, 32px)" -- Increased font size here
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


viewDigitButtons : SudokuGridFrontend -> Html FrontendMsg
viewDigitButtons grid =
    let
        allDigits =
            List.range 1 9

        availableDigits =
            allDigits
                |> List.filter (\digit -> not (isDigitSolved digit grid))
    in
    div
        [ Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(10, 1fr)"
        , Attr.style "width" "100%"
        , Attr.style "margin-top" "20px"
        , Attr.style "gap" "10px"
        ]
        (List.map
            (\digit ->
                if List.member digit availableDigits then
                    div
                        [ onClick (InputDigit digit)
                        , Attr.style "font-size" "28px"
                        , Attr.style "color" (Color.toHex Color.Text)
                        , Attr.style "cursor" "pointer"
                        , Attr.style "display" "flex"
                        , Attr.style "justify-content" "center"
                        , Attr.style "align-items" "center"
                        , Attr.style "user-select" "none"
                        ]
                        [ text (String.fromInt digit) ]

                else
                    div [] []
             -- Empty div to maintain spacing
            )
            allDigits
            ++ [ div
                    [ onClick RemoveGuess
                    , Attr.style "font-size" "28px"
                    , Attr.style "color" (Color.toHex Color.Text)
                    , Attr.style "cursor" "pointer"
                    , Attr.style "display" "flex"
                    , Attr.style "justify-content" "center"
                    , Attr.style "align-items" "center"
                    , Attr.style "user-select" "none"
                    ]
                    [ text "X" ]
               ]
        )


isDigitSolved : Int -> SudokuGridFrontend -> Bool
isDigitSolved digit grid =
    let
        flattenedGrid =
            List.concat grid

        solvedCount =
            List.filter
                (\cellState ->
                    case cellState of
                        NotChangeable n ->
                            n == digit

                        Changeable n ->
                            n == digit

                        _ ->
                            False
                )
                flattenedGrid
                |> List.length
    in
    solvedCount == 9


viewConnectedPlayers : List Player -> Html FrontendMsg
viewConnectedPlayers players =
    div
        [ Attr.style "margin-top" "20px"
        , Attr.style "color" (Color.toHex Color.Text)
        , Attr.style "font-size" "14px"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "row"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "flex-wrap" "wrap"
        ]
        (List.map viewPlayer players)


viewPlayer : Player -> Html FrontendMsg
viewPlayer player =
    div
        [ Attr.style "margin-bottom" "3px"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        ]
        [ text (Maybe.withDefault (String.left 4 player.sessionId) player.name)
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


viewCurrentPlayer : Maybe Player -> Html FrontendMsg
viewCurrentPlayer maybePlayer =
    div
        [ Attr.style "margin-bottom" "10px"
        , Attr.style "color" (Color.toHex Color.Text)
        , Attr.style "font-size" "16px"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        ]
        [ case maybePlayer of
            Just player ->
                let
                    displayName =
                        Maybe.withDefault (String.left 4 player.sessionId) player.name
                in
                div
                    [ Attr.style "display" "flex"
                    , Attr.style "align-items" "center"
                    ]
                    [ span [] [ text displayName ]
                    , viewLifes player.lifes
                    , Ui.Button.view
                        { onClick = OpenNamePopover
                        , label =
                            if player.name == Nothing then
                                "Set name"

                            else
                                "Change name"
                        }
                    ]

            Nothing ->
                text "Not connected"
        ]



-- Add this helper function


playerCanMakeGuess : Maybe Player -> Bool
playerCanMakeGuess maybePlayer =
    case maybePlayer of
        Just player ->
            player.lifes /= Nothing

        Nothing ->
            False


viewGameOverPopover : Bool -> Html FrontendMsg
viewGameOverPopover isVisible =
    Ui.Popover.view isVisible
        [ h2 [] [ text "Game Over" ]
        , p [] [ text "You've lost all your lives!" ]
        , Ui.Button.view
            { onClick = CloseGameOverPopover
            , label = "Close"
            }
        ]


viewNamePopover : Bool -> String -> Html FrontendMsg
viewNamePopover isVisible nameInput =
    Ui.Popover.view isVisible
        [ h2 [] [ text "Enter Your Name" ]
        , Ui.Input.view
            { value = nameInput
            , placeholder = "Your name"
            , onInput = UpdateNameInput
            }
        , div []
            [ Ui.Button.view
                { onClick = SaveName
                , label = "Save"
                }
            , Ui.Button.view
                { onClick = CloseNamePopover
                , label = "Cancel"
                }
            ]
        ]
