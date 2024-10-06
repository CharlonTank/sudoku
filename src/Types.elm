module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import Random
import Time
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, int, map, oneOf, s, string)


type Route
    = Home
    | Admin


type alias SudokuGridBackend =
    List (List DigitValueBackend)


type alias DigitValueBackend =
    { cellState : CellStateBackend, value : Int }


type CellStateBackend
    = RevealedCell
    | Guess Int
    | IncorrectGuess Int -- Changed from WrongGuess to IncorrectGuess
    | EmptyCell


type alias SudokuGridFrontend =
    List (List CellStateFrontend)


type CellStateFrontend
    = NotChangeable Int
    | Changeable Int
    | WrongGuess Int
    | NoValue


type alias FrontendModel =
    { key : Key
    , grid : Maybe SudokuGridFrontend
    , selectedCell : Maybe Position
    , connectedPlayers : List Player
    , currentPlayer : Maybe Player
    , gameoverPopoverOn : Bool
    , namePopoverOn : Bool
    , nameInput : String
    , route : Route -- Add this line
    }


type alias BackendModel =
    { grid : Maybe SudokuGridBackend
    , seed : Random.Seed
    , connectedPlayers : List Player
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | SelectCell Int Int
    | InputDigit Int
    | KeyPressed String
    | RemoveGuess
    | CloseGameOverPopover
    | OpenNamePopover
    | CloseNamePopover
    | UpdateNameInput String
    | SaveName
    | ResetBackend


type ToBackend
    = UpdateCell Position Int
    | RemoveCellValue Position
    | UpdatePlayerName String
    | ResetBackendRequest


type BackendMsg
    = NoOpBackendMsg
    | NewSudokuGridBackendMsg SudokuGridBackend Random.Seed
    | ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId
    | InitialTime Int
    | UpdatePlayerNameBackend SessionId String
    | PerformBackendReset


type ToFrontend
    = NewSudokuGridToFrontend SudokuGridFrontend
    | UpdatedUserGridToFrontend SudokuGridFrontend
    | ConnectedPlayersChanged (List Player)
    | SetCurrentPlayer Player
    | PlayerNameUpdated Player
    | BackendResetConfirmation


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


sudokuGridToFrontend : SudokuGridBackend -> SudokuGridFrontend
sudokuGridToFrontend =
    List.map (List.map cellStateToFrontend)


type alias Position =
    ( Int, Int )


type alias Player =
    { sessionId : SessionId
    , lifes : Maybe Life
    , name : Maybe String -- Add this line
    }


type Life
    = ThreeLife
    | TwoLife
    | OneLife


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home Url.Parser.top
        , map Admin (s "admin")
        ]
