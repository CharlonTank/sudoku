module Evergreen.V2.Types exposing (..)

import Browser
import Browser.Navigation
import Lamdera
import Random
import Url


type CellStateFrontend
    = NotChangeable Int
    | Changeable Int
    | NoValue


type alias SudokuGridFrontend =
    List (List CellStateFrontend)


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , grid : Maybe SudokuGridFrontend
    , selectedCell : Maybe ( Int, Int )
    }


type CellStateBackend
    = RevealedCell
    | Guess Int
    | EmptyCell


type alias DigitValueBackend =
    { cellState : CellStateBackend
    , value : Int
    }


type alias SudokuGridBackend =
    List (List DigitValueBackend)


type alias BackendModel =
    { grid : Maybe SudokuGridBackend
    , seed : Random.Seed
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | SelectCell Int Int
    | InputDigit Int
    | KeyPressed String
    | RemoveGuess


type ToBackend
    = UpdateCell Int Int Int
    | RemoveCellValue Int Int


type BackendMsg
    = NoOpBackendMsg
    | NewSudokuGridBackendMsg SudokuGridBackend Random.Seed
    | OnConnect Lamdera.SessionId Lamdera.ClientId
    | InitialTime Int


type ToFrontend
    = NewSudokuGridToFrontend SudokuGridFrontend
    | UpdatedUserGridToFrontend SudokuGridFrontend
