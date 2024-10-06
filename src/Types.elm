module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import Random
import Url exposing (Url)


type alias SudokuGridBackend =
    List (List DigitValueBackend)


type alias DigitValueBackend =
    { cellState : CellStateBackend, value : Int }


type CellStateBackend
    = RevealedCell
    | Guess Int
    | EmptyCell


type alias SudokuGridFrontend =
    List (List CellStateFrontend)


type CellStateFrontend
    = NotChangeable Int
    | Changeable Int
    | NoValue


type alias FrontendModel =
    { key : Key
    , grid : Maybe SudokuGridFrontend
    , selectedCell : Maybe Position
    , connectedSessions : List SessionId
    }


type alias BackendModel =
    { grid : Maybe SudokuGridBackend
    , seed : Random.Seed
    , connectedSessions : List SessionId
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | SelectCell Int Int
    | InputDigit Int
    | KeyPressed String
    | RemoveGuess


type ToBackend
    = UpdateCell Position Int
    | RemoveCellValue Position


type BackendMsg
    = NoOpBackendMsg
    | NewSudokuGridBackendMsg SudokuGridBackend Random.Seed
    | ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId
    | InitialTime Int


type ToFrontend
    = NewSudokuGridToFrontend SudokuGridFrontend
    | UpdatedUserGridToFrontend SudokuGridFrontend
    | ConnectedSessionsChanged (List SessionId)


cellStateToFrontend : DigitValueBackend -> CellStateFrontend
cellStateToFrontend { cellState, value } =
    case cellState of
        RevealedCell ->
            NotChangeable value

        Guess guessValue ->
            Changeable guessValue

        EmptyCell ->
            NoValue


sudokuGridToFrontend : SudokuGridBackend -> SudokuGridFrontend
sudokuGridToFrontend =
    List.map (List.map cellStateToFrontend)


type alias Position =
    ( Int, Int )
