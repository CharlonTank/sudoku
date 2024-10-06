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
      }
    , Task.perform (\posix -> InitialTime (Time.posixToMillis posix)) Time.now
    )


subscriptions : BackendModel -> Sub BackendMsg
subscriptions model =
    Lamdera.onConnect OnConnect


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

        OnConnect _ clientId ->
            case model.grid of
                Just grid ->
                    ( model
                    , sendToFrontend clientId (NewSudokuGridToFrontend (sudokuGridToFrontend grid))
                    )

                Nothing ->
                    ( model, Cmd.none )

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
        UpdateCell row col value ->
            handleCellUpdate row col (Guess value) model

        RemoveCellValue row col ->
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
                                    , broadcast (NewSudokuGridToFrontend (SudokuLogic.sudokuGridToFrontend brandNewGrid))
                                    )

                                else
                                    ( newModel, broadcast (UpdatedUserGridToFrontend (SudokuLogic.sudokuGridToFrontend newGrid)) )

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
