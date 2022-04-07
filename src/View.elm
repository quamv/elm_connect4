{-
   view functions

   clueless groping here. constantly working against the flow.

   TODO: redesign entirely. Connect4, should be html only, no SVG
-}


module View exposing (view)

import Array exposing (..)
import C4Board exposing (..)
import Html exposing (Html, a, button, div, fieldset, h1, h3, input, label, li, p, section, span, text, ul)
import Html.Attributes exposing (checked, href, placeholder, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import List exposing (..)
import Model exposing (..)
import Set exposing (toList)
import Svg exposing (circle, defs, g, image, line, marker, path, rect, svg, text_)
import Svg.Attributes
    exposing
        ( color
        , cx
        , cy
        , fill
        , fontSize
        , height
        , id
        , opacity
        , r
        , stroke
        , strokeWidth
        , viewBox
        , width
        , x
        , y
        )


squareHeight =
    100


squareWidth =
    100


svgwidth =
    squareWidth * cols


svgheight =
    squareHeight * rows


defaultbg =
    "beige"


styles =
    { centered = [ style "margin" "0 auto" ]
    , bg = [ style "background" defaultbg ]
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
            [ style "overflow" "hidden" 
            , style "word-wrap" "break-word"
            , style "width" "600px"
            , style "padding" "10px"
            ]
            ++ styles.bg
    in
    div
        containerStyle
        [ gameOverView (model.gameState == GameOver) model.winner
        , gameView model
        ]


debugView : Model -> Html Msg
debugView model =
    div []
        [text "Debug view is not implemented on Elm 0.19.1"
        -- [ div [] [ text <| String.fromList model.selections ]
        -- , div [] [ text <| String. model.gameState ]
        -- , div [] [ text <| toString model.winner ]
        -- , div [] [ text <| toString model.squares ]
        ]


gameOverView : Bool -> Maybe PlayerSide -> Html Msg
gameOverView show winner =
    if show then
        div
            []
            [ h1 [] [ text "Game Over" ]
            , div []
                [ text <|
                    case winner of
                        Just PlayerSide1 ->
                            "Player 1"

                        Just PlayerSide2 ->
                            "Player 2"

                        Nothing ->
                            "A butterfly"
                ]
            ]

    else
        div [] []


simpleTextDiv : String -> String -> Html Msg
simpleTextDiv prefix str =
    div [] [ text <| prefix ++ str ]


gridRectStyle =
    [ ( "stroke", "black" )
    , ( "strokeWidth", "1" )
    , ( "fill", "blue" )
    ]


getCircFill : Maybe SquareState -> String
getCircFill state =
    case state of
        Nothing ->
            defaultbg

        Just PlayerSide1 ->
            "red"

        Just PlayerSide2 ->
            "yellow"


gridRow : Int -> Int -> Connect4Board -> List (Svg.Svg Msg)
gridRow row col squares =
    if col == cols then
        []

    else
        let
            thisrect =
                rect
                    [ x <| String.fromInt (col * squareWidth)
                    , y <| String.fromInt (row * squareHeight)
                    , width <| String.fromInt squareWidth
                    , height <| String.fromInt squareHeight
                    , stroke "black"
                    , strokeWidth "1"
                    , fill "blue"
                    ]
                    []

            circfill =
                getCircFill <| getSquareState row col squares

            thiscirc =
                circle
                    [ cx (String.fromFloat <| (toFloat (col * squareWidth) + (squareWidth / 2)))
                    , cy (String.fromFloat <| (toFloat (row * squareHeight) + (squareHeight / 2)))
                    , r <| String.fromFloat (squareHeight / 3)
                    , fill circfill
                    ]
                    []
        in
        [ thisrect, thiscirc ]
            ++ gridRow row (col + 1) squares


gridView : Int -> Connect4Board -> List (Svg.Svg Msg)
gridView row squares =
    if row >= rows then
        [ g [] [] ]

    else
        gridRow row 0 squares
            ++ gridView (row + 1) squares


gameContainerStyle =
        [ style "background-color" "beige" 
        , style "margin-top" (String.fromInt squareHeight ++ "px")
        ]


colHdrItemStyle =
        [ style "text-align" "center"
        , style "width" "83px"
        , style "height" "100px"
        , style "list-style-type" "none"
        , style "border" "1px solid black"
        , style "float" "left"
        ]


colHdrRow : Html Msg
colHdrRow =
    ul
        [ {-
             ("display","flex"),
             ("justify-content","space-around"),
             ("border","1px solid black"),
          -}
          style "border" "0"
        , style "padding" "0"
        , style "margin" "0"
        , style "width" "100%"
        ]
        [ li colHdrItemStyle [ h3 [] [ text "1" ] ]
        , li colHdrItemStyle [ h3 [] [ text "2" ] ]
        , li colHdrItemStyle [ h3 [] [ text "3" ] ]
        , li colHdrItemStyle [ h3 [] [ text "4" ] ]
        , li colHdrItemStyle [ h3 [] [ text "5" ] ]
        , li colHdrItemStyle [ h3 [] [ text "6" ] ]
        , li colHdrItemStyle [ h3 [] [ text "7" ] ]
        ]


gameView : Model -> Html Msg
gameView model =
    let
        viewboxstr =
            "0 0 " ++ String.fromInt svgwidth ++ " " ++ String.fromInt svgheight

        innersvgnodes =
            [ backgroundView
            , squaresView model
            ]
                ++ gridView 0 model.squares

        svgnode =
            svg [ viewBox viewboxstr ] innersvgnodes
    in
    div
        gameContainerStyle
        [ colHdrRow
        , svgnode
        ]


squaresView : Model -> Html Msg
squaresView model =
    g [] <|
        Array.toList (Array.indexedMap singleSquareView model.squares)


squareFill : Maybe SquareState -> String
squareFill state =
    case state of
        Nothing ->
            "none"

        Just PlayerSide1 ->
            "red"

        Just PlayerSide2 ->
            "blue"


singleSquareView : Int -> Maybe SquareState -> Html Msg
singleSquareView idx squarestate =
    let
        squarefill =
            squareFill squarestate

        row =
            idx // cols

        col =
            remainderBy cols idx

        squareX =
            round <| toFloat (col * squareWidth)

        squareY =
            round <| toFloat (row * squareWidth)
    in
    square
        [ x (String.fromInt squareX)
        , y (String.fromInt squareY)
        , fill squarefill
        ]


square : List (Svg.Attribute msg) -> Svg.Svg msg
square attrs =
    rect
        (attrs ++ squareStyle2)
        []


squareStyle2 =
    [ width (String.fromInt squareWidth)
    , height (String.fromInt squareHeight)
    ]


backgroundView : Html msg
backgroundView =
    rect
        [ x "0"
        , y "0"
        , Svg.Attributes.width (String.fromInt svgwidth)
        , Svg.Attributes.height (String.fromInt svgheight)
        , fill "grey"
        ]
        []
