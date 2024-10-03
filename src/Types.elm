module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import Url exposing (Url)


type alias SudokuGrid =
    List (List Int)


type alias FrontendModel =
    { key : Key
    , message : String
    , sudokuGrid : SudokuGrid
    , userGrid : SudokuGrid
    }


type alias BackendModel =
    { message : String
    , sudokuGrid : SudokuGrid
    , userGrid : SudokuGrid
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | UserInput Int Int String


type ToBackend
    = UpdateCell Int Int Int


type BackendMsg
    = NoOpBackendMsg
    | NewSudokuGridBackendMsg SudokuGrid
    | OnConnect SessionId ClientId


type ToFrontend
    = NewSudokuGridToFrontend SudokuGrid
    | UpdatedUserGridToFrontend SudokuGrid
