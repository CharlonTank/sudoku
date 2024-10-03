module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Lamdera
import Url


type alias SudokuGrid =
    List (List Int)


type alias FrontendModel =
    { key : Browser.Navigation.Key
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
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | UserInput Int Int String


type ToBackend
    = UpdateCell Int Int Int


type BackendMsg
    = NoOpBackendMsg
    | NewSudokuGridBackendMsg SudokuGrid
    | OnConnect Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NewSudokuGridToFrontend SudokuGrid
    | UpdatedUserGridToFrontend SudokuGrid
