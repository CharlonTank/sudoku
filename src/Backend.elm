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
      , seed = Random.initialSeed 42
      , connectedPlayers = []
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
            if List.any (\player -> player.sessionId == sessionId) model.connectedPlayers then
                ( model, Cmd.none )

            else
                let
                    newPlayer : Player
                    newPlayer =
                        { sessionId = sessionId, lifes = Just ThreeLife, name = Nothing }

                    newModel =
                        { model | connectedPlayers = newPlayer :: model.connectedPlayers }
                in
                ( newModel
                , Cmd.batch
                    [ broadcast (ConnectedPlayersChanged newModel.connectedPlayers)
                    , sendToFrontend clientId (SetCurrentPlayer newPlayer)
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
                    { model | connectedPlayers = List.filter (\player -> player.sessionId /= sessionId) model.connectedPlayers }
            in
            ( newModel
            , broadcast (ConnectedPlayersChanged newModel.connectedPlayers)
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

        UpdatePlayerNameBackend sessionId name ->
            let
                updatedPlayers =
                    List.map
                        (\player ->
                            if player.sessionId == sessionId then
                                { player | name = Just name }

                            else
                                player
                        )
                        model.connectedPlayers
            in
            ( { model | connectedPlayers = updatedPlayers }
            , broadcast (ConnectedPlayersChanged updatedPlayers)
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        UpdateCell position value ->
            let
                ( row, col ) =
                    position
            in
            handleCellUpdate sessionId row col (Guess value) model

        RemoveCellValue position ->
            let
                ( row, col ) =
                    position
            in
            handleCellUpdate sessionId row col EmptyCell model

        UpdatePlayerName name ->
            let
                updatedPlayers =
                    List.map
                        (\player ->
                            if player.sessionId == sessionId then
                                { player | name = Just name }

                            else
                                player
                        )
                        model.connectedPlayers

                updatedPlayer =
                    List.filter (\p -> p.sessionId == sessionId) updatedPlayers
                        |> List.head
            in
            ( { model | connectedPlayers = updatedPlayers }
            , Cmd.batch
                [ broadcast (ConnectedPlayersChanged updatedPlayers)
                , case updatedPlayer of
                    Just player ->
                        broadcast (PlayerNameUpdated player)

                    Nothing ->
                        Cmd.none
                ]
            )


handleCellUpdate : SessionId -> Int -> Int -> CellStateBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
handleCellUpdate sessionId row col newCellState model =
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
                                    isCorrect =
                                        case newCellState of
                                            Guess guessValue ->
                                                guessValue == cell.value

                                            _ ->
                                                True

                                    updatedCellState =
                                        if isCorrect then
                                            newCellState

                                        else
                                            case newCellState of
                                                Guess guessValue ->
                                                    IncorrectGuess guessValue

                                                _ ->
                                                    newCellState

                                    newGrid =
                                        SudokuLogic.updateGrid row col { cellState = updatedCellState, value = cell.value } grid

                                    ( updatedPlayers, lifeLost ) =
                                        if not isCorrect then
                                            updatePlayerLife sessionId model.connectedPlayers

                                        else
                                            ( model.connectedPlayers, False )

                                    newModel =
                                        { model | grid = Just newGrid, connectedPlayers = updatedPlayers }
                                in
                                if SudokuLogic.isSudokuComplete newGrid then
                                    let
                                        ( brandNewGrid, newSeed ) =
                                            SudokuLogic.generateSudoku model.seed
                                    in
                                    ( { newModel | grid = Just brandNewGrid, seed = newSeed }
                                    , Cmd.batch
                                        [ broadcast (NewSudokuGridToFrontend (sudokuGridToFrontend brandNewGrid))
                                        , broadcast (ConnectedPlayersChanged updatedPlayers)
                                        ]
                                    )

                                else
                                    ( newModel
                                    , Cmd.batch
                                        [ broadcast (UpdatedUserGridToFrontend (sudokuGridToFrontend newGrid))
                                        , if lifeLost then
                                            broadcast (ConnectedPlayersChanged updatedPlayers)

                                          else
                                            Cmd.none
                                        ]
                                    )

                    Nothing ->
                        ( model, Cmd.none )
            )
        |> Maybe.withDefault ( model, Cmd.none )


updatePlayerLife : SessionId -> List Player -> ( List Player, Bool )
updatePlayerLife sessionId players =
    let
        updateLife player =
            if player.sessionId == sessionId then
                case player.lifes of
                    Just ThreeLife ->
                        ( { player | lifes = Just TwoLife }, True )

                    Just TwoLife ->
                        ( { player | lifes = Just OneLife }, True )

                    Just OneLife ->
                        ( { player | lifes = Nothing }, True )

                    Nothing ->
                        ( player, False )

            else
                ( player, False )

        ( updatedPlayers, lifeLostList ) =
            List.unzip (List.map updateLife players)
    in
    ( updatedPlayers, List.any identity lifeLostList )


sudokuGridToFrontend : SudokuGridBackend -> SudokuGridFrontend
sudokuGridToFrontend =
    List.map (List.map cellStateToFrontend)


cellStateToFrontend : DigitValueBackend -> CellStateFrontend
cellStateToFrontend { cellState, value } =
    case cellState of
        RevealedCell ->
            NotChangeable value

        Guess guessValue ->
            if guessValue == value then
                Changeable guessValue

            else
                WrongGuess guessValue

        IncorrectGuess guessValue ->
            WrongGuess guessValue

        EmptyCell ->
            NoValue
