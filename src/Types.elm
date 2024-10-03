module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId)
import Url exposing (Url)


type alias SudokuGrid =
    List (List Int)


type alias FrontendModel =
    { key : Key
    , message : String
    , sudokuGrid : SudokuGrid
    , userGrid : SudokuGrid -- New field to store user input
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | GenerateSudoku
    | UserInput Int Int String -- New message for user input


type ToBackend
    = RequestNewSudoku


type BackendMsg
    = NoOpBackendMsg
    | NewSudokuGridBackendMsg ClientId SudokuGrid


type ToFrontend
    = NewSudokuGridToFrontend SudokuGrid
