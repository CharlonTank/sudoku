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
    , selectedCell : Maybe ( Int, Int )
    }


type alias BackendModel =
    { grid : Maybe SudokuGridBackend
    , seed : Random.Seed
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | SelectCell Int Int
    | InputDigit Int
    | KeyPressed String


type ToBackend
    = UpdateCell Int Int Int


type BackendMsg
    = NoOpBackendMsg
    | NewSudokuGridBackendMsg SudokuGridBackend Random.Seed
    | OnConnect SessionId ClientId
    | InitialTime Int


type ToFrontend
    = NewSudokuGridToFrontend SudokuGridFrontend
    | UpdatedUserGridToFrontend SudokuGridFrontend


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
