{-
view functions

clueless groping here. constantly working against the flow.

TODO: redesign entirely
-}
module View exposing (view)

import Array exposing (..)
import List exposing (..)
import Html exposing (Html, div, p, text, a, button, h3, label, input, ul, fieldset, section, span,h1, li)
import Html.Attributes exposing (style, href, target, type_, value, checked, placeholder)
import Html.Events exposing (onClick, onInput)
import Svg exposing (svg, rect, image, g, circle, line, text_, marker, path, defs)
import Svg.Attributes exposing (id, x, y, viewBox, fill, width, height,
    cx, cy, r, color, stroke, strokeWidth, fontSize, opacity)
import Set exposing (toList)

import Model exposing (..)
import C4Board exposing (..)

defaultbg = "beige"

styles = {
    centered =  [("margin","0 auto")]
    ,bg = [("background",defaultbg)]
    }

getSquareState : Int -> Int -> Connect4Board -> Maybe SquareState
getSquareState row col squares =
    case Array.get (row * cols + col) squares of
        Nothing ->
            -- could return this if the value is Nothing or if the
            -- idx is out of range
            Nothing

        Just val ->
            -- the contents of the array are maybes
            -- this is confusing.
            val


view : Model -> Html Msg
view model =
    let
        containerStyle =
            [
                ("overflow","hidden"),
                ("word-wrap","break-word"),
                ("width","600px"),
                ("padding","10px")
            ]
            ++ styles.bg
    in
        div
            [
                style (styles.centered ++ containerStyle)
            ]
            [
                gameOverView (model.gameState == GameOver) model.winner
                ,gameView model
            ]

debugView : Model -> Html Msg
debugView model =
    div [] [
        div [] [ text <| toString model.selections ]
        , div [] [ text <| toString model.gameState ]
        , div [] [ text <| toString model.winner ]
        , div [] [ text <| toString model.squares ]
    ]

gameOverView : Bool -> Maybe PlayerSide -> Html Msg
gameOverView show winner =
    if show then
        div
            []
            [
                h1 [] [ text "Game Over" ]
                ,div [] [
                    text <| case winner of
                        Just PlayerSide1 -> "Player 1"
                        Just PlayerSide2 -> "Player 2"
                        Nothing -> "A butterfly"
                ]
            ]
    else
        div [] []

simpleTextDiv : String -> String -> Html Msg
simpleTextDiv prefix str =
    div [] [ text <| prefix ++ str]

gridRectStyle =
    [
        ("stroke","black")
        ,("strokeWidth","1")
        ,("fill","blue")
    ]


getCircFill : Maybe SquareState -> String
getCircFill state =
    case state of
        Nothing -> defaultbg
        Just PlayerSide1 -> "red"
        Just PlayerSide2 -> "yellow"


gridRow : Int -> Int -> Connect4Board -> List (Svg.Svg Msg)
gridRow row col squares =
    if col == cols then
        []
    else
        let
            thisrect =
                rect
                    [
                        x <| toString (col * squareWidth)
                        , y <| toString (row * squareHeight)
                        , width <| toString squareWidth
                        , height <| toString squareHeight
                        , stroke "black"
                        , strokeWidth "1"
                        , fill "blue"
                    ]
                    []

            circfill =
                getCircFill <| getSquareState row col squares

            thiscirc =
                circle
                    [
                        cx (toString <| (toFloat (col * squareWidth) + (squareWidth / 2)))
                        ,cy (toString <| (toFloat (row * squareHeight) + (squareHeight / 2)))
                        ,r <| toString (squareHeight / 3)
                        ,fill circfill
                    ]
                    []
        in
            [thisrect, thiscirc]
            ++ (gridRow row (col + 1) squares)


gridView : Int -> Connect4Board -> List (Svg.Svg Msg)
gridView row squares =
    if row >= rows then
        [g [] []]
    else
        (gridRow row 0 squares)
        ++ (gridView (row+1) squares)

gameContainerStyle =
    style [
        ("background-color","beige")
        ,("margin-top", (toString squareHeight) ++ "px")
    ]

colHdrItemStyle =
    style [
        ("text-align","center")
        ,("width","83px")
        ,("height","100px")
        ,("list-style-type","none")
        ,("border","1px solid black")
        ,("float","left")
    ]

colHdrRow : Html Msg
colHdrRow =
    ul
        [
            style [
{-
                ("display","flex"),
                ("justify-content","space-around"),
                ("border","1px solid black"),
-}
                ("border","0"),
                ("padding","0"),
                ("margin","0"),
                ("width","100%")]
            ]
        [
            li [colHdrItemStyle] [ h3 [] [ text "1"] ]
            ,li [colHdrItemStyle] [ h3 [] [ text "2"] ]
            ,li [colHdrItemStyle] [ h3 [] [ text "3"] ]
            ,li [colHdrItemStyle] [ h3 [] [ text "4"] ]
            ,li [colHdrItemStyle] [ h3 [] [ text "5"] ]
            ,li [colHdrItemStyle] [ h3 [] [ text "6"] ]
            ,li [colHdrItemStyle] [ h3 [] [ text "7"] ]
        ]


gameView : Model -> Html Msg
gameView model =
    let
        viewboxstr =
            "0 0 " ++ (toString svgwidth) ++ " " ++ (toString svgheight)

        innersvgnodes =
            [
                backgroundView
                ,squaresView model
            ]
            ++ gridView 0 model.squares

        svgnode =
            svg [viewBox viewboxstr] innersvgnodes
    in
        div
            [ gameContainerStyle ]
            [
                colHdrRow
                ,svgnode
            ]



squaresView : Model -> Html Msg
squaresView model =
    g [] <|
        Array.toList (Array.indexedMap singleSquareView model.squares)


squareFill : Maybe SquareState -> String
squareFill state =
    case state of
        Nothing -> "none"
        Just PlayerSide1 -> "red"
        Just PlayerSide2 -> "blue"


singleSquareView : Int -> Maybe SquareState -> Html Msg
singleSquareView idx squarestate =
    let
        squarefill = squareFill squarestate
        row = idx // cols
        col = rem idx cols
        squareX = round <| toFloat (col * squareWidth)
        squareY = round <| toFloat (row * squareWidth)
    in
        square
            [
                x (toString squareX)
                , y (toString squareY)
                , fill squarefill
            ]

square : List (Svg.Attribute msg) -> Svg.Svg msg
square attrs =
    rect
        (attrs ++ squareStyle2)
        []

squareStyle2 =
    [
        width (toString squareWidth)
        ,height (toString squareHeight)
    ]

backgroundView : Html msg
backgroundView =
    rect
        [
            x "0"
            , y "0"
            , Svg.Attributes.width (toString svgwidth)
            , Svg.Attributes.height (toString svgheight)
            , fill "grey"
        ]
        []


