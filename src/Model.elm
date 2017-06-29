module Model exposing (..)

import Array exposing (Array)
import Keyboard exposing (KeyCode)

type GameState = Normal | GameOver
type Msg = KeyDownMsg Keyboard.KeyCode
type PlayerSide = PlayerSide1 | PlayerSide2
type alias SquareState = PlayerSide
type alias Connect4Board = Array (Maybe SquareState)

type alias Model = {
    gameState: GameState
    , currentPlayer: PlayerSide
    , squares: Connect4Board
    , selections: List Int
    , winner: Maybe PlayerSide
    }

rows = 6
cols = 7

