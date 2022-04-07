module Model exposing (..)

import Array exposing (Array)


type GameState
    = Normal
    | GameOver

type KeyEventMsg
    = KeyEventControl
    | KeyEventAlt
    | KeyEventShift
    | KeyEventMeta
    | KeyEventLetter Char
    | KeyEventUnknown String


type Msg
    = KeyPressed String


type PlayerSide
    = PlayerSide1
    | PlayerSide2


type alias SquareState =
    PlayerSide


type alias Connect4Board =
    Array (Maybe SquareState)


type alias Model =
    { gameState : GameState
    , currentPlayer : PlayerSide
    , squares : Connect4Board
    , selections : List Int
    , winner : Maybe PlayerSide
    }


rows: Int
rows = 6

cols : Int
cols = 7
