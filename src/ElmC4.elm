
module ElmC4 exposing (main)

import Html exposing (..)
import Keyboard exposing (..)
import Char exposing (toCode, fromCode)
import String exposing (fromChar)
import Set exposing (..)
import Array exposing (..)

import Model exposing (..)
import View exposing (..)
import C4Board exposing (..)


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
        (model, Cmd.none)
    else if fromCode k == 'U' then
        undo model
    else
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
        -- user/internal column numbers differ by 1. ie.
        -- when user chooses column '1', internally that means col 0
        key = fromCode ((toCode key1) - 1)
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
            let
                newselections =
                    -- keep the rest of the list
                    taillist

                newsquares =
                    -- clear the most recently selected square
                    Array.set head Nothing model.squares

                nextplayer =
                    -- each undo should swap the currentplayer
                    togglePlayer model.currentPlayer
            in
                ({model |
                    selections = newselections
                    ,currentPlayer = nextplayer
                    ,squares = newsquares}
                 , Cmd.none)


{-
swap PlayerSide1->PlayerSide2 and vice versa
-}
togglePlayer : PlayerSide -> PlayerSide
togglePlayer player =
    case player of
        PlayerSide1 -> PlayerSide2
        PlayerSide2 -> PlayerSide1
