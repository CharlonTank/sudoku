module Backend exposing (..)

import Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontend)
import Random
import SudokuLogic
import Task
import Time
import Types exposing (..)


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { grid = Nothing
      , seed = Random.initialSeed 0
      , connectedSessions = []
      }
    , Task.perform (\posix -> InitialTime (Time.posixToMillis posix)) Time.now
    )


subscriptions : BackendModel -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        ]


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        NewSudokuGridBackendMsg grid newSeed ->
            let
                newModel =
                    { model | grid = Just grid, seed = newSeed }
            in
            ( newModel
            , broadcast (NewSudokuGridToFrontend (sudokuGridToFrontend grid))
            )

        ClientConnected sessionId clientId ->
            if List.member sessionId model.connectedSessions then
                -- Session already connected, no need to update
                ( model, Cmd.none )

            else
                let
                    newModel =
                        { model | connectedSessions = sessionId :: model.connectedSessions }
                in
                ( newModel
                , Cmd.batch
                    [ broadcast (ConnectedSessionsChanged newModel.connectedSessions)
                    , case model.grid of
                        Just grid ->
                            sendToFrontend clientId (NewSudokuGridToFrontend (sudokuGridToFrontend grid))

                        Nothing ->
                            Cmd.none
                    ]
                )

        ClientDisconnected sessionId clientId ->
            let
                newModel =
                    { model | connectedSessions = List.filter (\id -> id /= sessionId) model.connectedSessions }
            in
            ( newModel
            , broadcast (ConnectedSessionsChanged newModel.connectedSessions)
            )

        InitialTime time ->
            let
                initialSeed =
                    Random.initialSeed time

                ( newGrid, newSeed ) =
                    SudokuLogic.generateSudoku initialSeed
            in
            ( { model | grid = Just newGrid, seed = newSeed }
            , broadcast (NewSudokuGridToFrontend (sudokuGridToFrontend newGrid))
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        UpdateCell position value ->
            let
                ( row, col ) =
                    position
            in
            handleCellUpdate row col (Guess value) model

        RemoveCellValue position ->
            let
                ( row, col ) =
                    position
            in
            handleCellUpdate row col EmptyCell model


handleCellUpdate : Int -> Int -> CellStateBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
handleCellUpdate row col newCellState model =
    model.grid
        |> Maybe.map
            (\grid ->
                case
                    List.drop row grid
                        |> List.head
                        |> Maybe.andThen (List.drop col >> List.head)
                of
                    Just cell ->
                        case cell.cellState of
                            RevealedCell ->
                                ( model, Cmd.none )

                            _ ->
                                let
                                    newGrid =
                                        SudokuLogic.updateGrid row col { cellState = newCellState, value = cell.value } grid

                                    newModel =
                                        { model | grid = Just newGrid }
                                in
                                if SudokuLogic.isSudokuComplete newGrid then
                                    let
                                        ( brandNewGrid, newSeed ) =
                                            SudokuLogic.generateSudoku model.seed
                                    in
                                    ( { newModel | grid = Just brandNewGrid, seed = newSeed }
                                    , broadcast (NewSudokuGridToFrontend (sudokuGridToFrontend brandNewGrid))
                                    )

                                else
                                    ( newModel, broadcast (UpdatedUserGridToFrontend (sudokuGridToFrontend newGrid)) )

                    Nothing ->
                        ( model, Cmd.none )
            )
        |> Maybe.withDefault ( model, Cmd.none )


sudokuGridToFrontend : SudokuGridBackend -> SudokuGridFrontend
sudokuGridToFrontend =
    List.map (List.map cellStateToFrontend)


cellStateToFrontend : DigitValueBackend -> CellStateFrontend
cellStateToFrontend { cellState, value } =
    case cellState of
        RevealedCell ->
            NotChangeable value

        Guess guessValue ->
            Changeable guessValue

        EmptyCell ->
            NoValue
