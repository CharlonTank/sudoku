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
    ( { message = "Hello!"
      , sudokuGrid = List.repeat 9 (List.repeat 9 (Changeable 0))
      , userGrid = List.repeat 9 (List.repeat 9 (Changeable 0))
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
                    { model | sudokuGrid = grid, userGrid = grid, seed = newSeed }
            in
            ( newModel
            , broadcast (NewSudokuGridToFrontend grid)
            )

        OnConnect sessionId clientId ->
            ( model
            , sendToFrontend clientId (NewSudokuGridToFrontend model.userGrid)
            )

        InitialTime time ->
            let
                initialSeed =
                    Random.initialSeed time

                ( newGrid, newSeed ) =
                    SudokuLogic.generateSudoku initialSeed
            in
            ( { model | sudokuGrid = newGrid, userGrid = newGrid, seed = newSeed }
            , broadcast (NewSudokuGridToFrontend newGrid)
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        UpdateCell row col value ->
            let
                currentCell =
                    model.userGrid
                        |> List.drop row
                        |> List.head
                        |> Maybe.andThen (List.drop col >> List.head)
                        |> Maybe.withDefault (Changeable 0)
            in
            case currentCell of
                NotChangeable _ ->
                    -- If the cell is not changeable, don't update it
                    ( model, Cmd.none )

                Changeable _ ->
                    let
                        newUserGrid =
                            updateGrid row col (Changeable value) model.userGrid

                        updatedGrid =
                            SudokuLogic.updateCompletedSections newUserGrid

                        newModel =
                            { model | userGrid = updatedGrid }

                        cmd =
                            if SudokuLogic.isSudokuComplete updatedGrid then
                                let
                                    ( newGrid, newSeed ) =
                                        SudokuLogic.generateSudoku model.seed
                                in
                                Cmd.batch
                                    [ broadcast (NewSudokuGridToFrontend newGrid)
                                    , Random.generate (\_ -> NoOpBackendMsg) (Random.constant newSeed)
                                    ]

                            else
                                broadcast (UpdatedUserGridToFrontend updatedGrid)
                    in
                    ( newModel, cmd )


updateGrid : Int -> Int -> DigitValue -> SudokuGrid -> SudokuGrid
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
