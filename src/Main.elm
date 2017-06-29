
module Main exposing (main)

import Html exposing (..)
import Keyboard exposing (..)
import Char exposing (toCode, fromCode)
import String exposing (fromChar)
import Set exposing (..)
import Array exposing (..)

import Model exposing (..)
import View exposing (..)
import C4Board exposing (playerSelectsCol,newBoard,setSquareStateByIdx)
import Helpers exposing (..)

main =
    Html.program {
        init = init,
        view = view,
        update = update,
        subscriptions = subscriptions
        }


{-
Creates the initial model and random ball direction
-}
init : ( Model, Cmd Msg )
init =
    ({
        gameState = Normal
        , currentPlayer = PlayerSide1
        , squares = newBoard rows cols
        , selections = []
        , winner = Nothing
    }
    , Cmd.none)


{-
Subscriptions
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDownMsg ]


{-
Update
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDownMsg k ->
            keyDown k model


{-
Process a KeyDown message
If the key is a special command, run it
Otherwise store the key in our set of currently pressed keys
-}
keyDown : Keyboard.KeyCode -> Model -> (Model, Cmd Msg)
keyDown k model =
    if model.gameState == GameOver then
        -- game's over. ignore key press
        (model, Cmd.none)
    else if fromCode k == 'U' then
        -- undo requested
        undo model
    else
        -- see if it's a column # (1-7)
        case keyToCol <| fromCode k of
            Just col ->
                -- user selected column 'col'
                playerSelectsCol model col

            Nothing ->
                -- key pressed was not a column #. ignore.
                (model, Cmd.none)

{-
check if a key pressed is a known column
it's easier for a user to use column numbers starting with '1'
so we are translating the key pressed "down" 1 here
(1->0, ..., 7->6)
-}
keyToCol : Char -> Maybe Int
keyToCol key1 =
    let
        key =
            -- user/internal column numbers differ by 1. ie.
            -- when user chooses column '1', internally that means col 0
            fromCode ((toCode key1) - 1)
    in
        if key >= '0' && key <= '6' then
            Just <| (toCode key) - (toCode '0')
        else
            Nothing


{-
undo the last move
can be used repeatedly for multi-undo
-}
undo : Model -> (Model, Cmd Msg)
undo model =
    case model.selections of
        [] ->
            -- nothing to undo
            (model, Cmd.none)

        head::taillist ->
            -- at least one selection available to  undo
            let
                newsquares =
                    -- clear the most recently selected square
                    -- return updated board
                    setSquareStateByIdx head Nothing model.squares
            in
                ({model |
                    selections = taillist
                    ,currentPlayer = togglePlayer model.currentPlayer
                    ,squares = newsquares}
                 , Cmd.none)


