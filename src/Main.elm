{- 
    Connect4 in Elm
-}
module Main exposing (main)

import C4Board exposing (newBoard, playerSelectsCol, setSquareStateByIdx)
import Char exposing (fromCode, toCode)
import Helpers exposing (togglePlayer)
import Model exposing (..)
import View exposing (view)
import Browser
import Browser.Events
import Json.Decode as Decode

main : Program () Model Msg
main = Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }

init : () -> (Model, Cmd Msg)
init _ =
    ( { gameState = Normal
      , currentPlayer = PlayerSide1
      , squares = newBoard rows cols
      , selections = []
      , winner = Nothing
      }
    , Cmd.none
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
        Browser.Events.onKeyPress <| Decode.map KeyPressed <| Decode.field "key" Decode.string
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of KeyPressed k -> keyDown k model

{-
   Process a KeyDown message
   If the key is a special command, run it
   Otherwise store the key in our set of currently pressed keys
-}
keyDown : String -> Model -> ( Model, Cmd Msg )
keyDown k model =
    let
        charpressed = decodeCharPressed k 
    in
    if model.gameState == GameOver then
        -- game's over. ignore key press
        ( model, Cmd.none )
    else if charpressed == 'U' then
        -- undo requested
        undo model
    else
        -- see if it's a column # (1-7)
        case keyToCol <| charpressed  of
            Just col ->
                -- user selected column 'col'
                playerSelectsCol model col

            Nothing ->
                -- key pressed was not a column #. ignore.
                ( model, Cmd.none )


decodeCharPressed: String -> Char
decodeCharPressed s =
    if s == "1" then '0'
    else if s == "2" then '1'
    else if s == "3" then '2'
    else if s == "4" then '3'
    else if s == "5" then '4'
    else if s == "6" then '5'
    else if s == "7" then '6'
    else 'z'


{-
   check if a key pressed is a known column
   it's easier for a user to use column numbers starting with '1'
   so we are translating the key pressed "down" 1 here
   (1->0, ..., 7->6)
-}
keyToCol : Char -> Maybe Int
keyToCol key1 =
    let
        key = key1
            -- user/internal column numbers differ by 1. ie.
            -- when user chooses column '1', internally that means col 0
            --fromCode (toCode key1 - 1)
    in
    if key >= '0' && key <= '6' then
        Just <| toCode key - toCode '0'

    else
        Nothing


{-
   undo the last move
   can be used repeatedly for multi-undo
-}
undo : Model -> ( Model, Cmd Msg )
undo model =
    case model.selections of
        [] ->
            -- nothing to undo
            ( model, Cmd.none )

        head :: taillist ->
            -- at least one selection available to  undo
            let
                newsquares =
                    -- clear the most recently selected square
                    -- return updated board
                    setSquareStateByIdx head Nothing model.squares
            in
            ( { model
                | selections = taillist
                , currentPlayer = togglePlayer model.currentPlayer
                , squares = newsquares
              }
            , Cmd.none
            )
