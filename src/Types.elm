module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import Random
import Url exposing (Url)


type DigitValue
    = NotChangeable Int
    | Changeable Int


type alias SudokuGrid =
    List (List DigitValue)


type alias FrontendModel =
    { key : Key
    , message : String
    , sudokuGrid : SudokuGrid
    , userGrid : SudokuGrid
    , selectedCell : Maybe ( Int, Int )
    }


type alias BackendModel =
    { message : String
    , sudokuGrid : SudokuGrid
    , userGrid : SudokuGrid
    , seed : Random.Seed
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | SelectCell Int Int
    | InputDigit Int


type ToBackend
    = UpdateCell Int Int Int


type BackendMsg
    = NoOpBackendMsg
    | NewSudokuGridBackendMsg SudokuGrid Random.Seed
    | OnConnect SessionId ClientId
    | InitialTime Int


type ToFrontend
    = NewSudokuGridToFrontend SudokuGrid
    | UpdatedUserGridToFrontend SudokuGrid
