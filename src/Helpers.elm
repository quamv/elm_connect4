module Helpers exposing (
    togglePlayer
    )

import Model exposing (..)


{-
swap PlayerSide1->PlayerSide2 and vice versa
-}
togglePlayer : PlayerSide -> PlayerSide
togglePlayer player =
    case player of
        PlayerSide1 -> PlayerSide2
        PlayerSide2 -> PlayerSide1
