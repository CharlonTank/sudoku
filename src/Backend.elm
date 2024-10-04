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

        OnConnect sessionId clientId ->
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
                                                updateGrid row col { cellState = Guess value, value = cell.value } grid

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


updateGrid : Int -> Int -> DigitValueBackend -> SudokuGridBackend -> SudokuGridBackend
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
