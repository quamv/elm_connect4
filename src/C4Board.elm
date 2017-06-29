{-
Manages the Connect 4 board and detects winners
-}

module C4Board exposing (
    playerSelectsCol
    ,newBoard
    ,setSquareStateByIdx
    )

{- system imports -}
import Array exposing (..)
{- local imports -}
import Model exposing (..)
import Helpers exposing (togglePlayer)


{-
convenience alias
-}
type alias WinnerCheckFunction = Connect4Board -> Int -> SquareState -> Bool


{-
customizable game primary threshold
-}
connectWhat = 4

{-
when looking for streaks of length 'connectWhat', the current idx counts as '1'.
so in connect 4 that means, knowing what the current value is, we only need to
search 3 more locations in either direction to find a streak of 4.
-}
len2Check : Int
len2Check =
    connectWhat - 1


{-
create a Connect 4 Board (an array) with the given number of rows and columns
-}
newBoard : Int -> Int -> Connect4Board
newBoard boardrows boardcols  =
    Array.repeat (rows * cols) Nothing



{-
set the state of a square at the given raw index
-}
setSquareStateByIdx : Int -> Maybe SquareState -> Connect4Board -> Connect4Board
setSquareStateByIdx idx squarestate board =
    Array.set idx squarestate board

{-
extract the row and column indexes from a raw index
-}
toColRow : Int -> (Int, Int)
toColRow idx =
    (idx // cols, rem idx cols)


{-
helper to get the value stored at a given index rather than row,col
-}
getValByIdx : Int -> Connect4Board -> Maybe SquareState
getValByIdx idx board =
    let
        (row,col) =
            toColRow idx
    in
        getSquareState row col board

{-
square state retrieval function. we have a case of overloaded semantic
meaning here. a 'Nothing' can mean either that the value stored at the
idx defined by row,col is Nothing, OR it can mean that row,col is
outside the range of the array. this helper just cleans that up a bit for
callers. it still has the semantic lack of clarity, just makes
this process a bit less verbose.
-}
getSquareState : Int -> Int -> Connect4Board -> Maybe SquareState
getSquareState row col board =
    case Array.get (row * cols + col) board of
        Nothing ->
            Nothing

        Just mbval ->
            case mbval of
                Nothing -> Nothing
                Just val -> Just val


{-
the user has selected a column. attempt to allocate a square and select
it for the player.
-}
playerSelectsCol : Model -> Int -> (Model, Cmd Msg)
playerSelectsCol model col =
    case getAvailIdxT2B col model.squares Nothing of
        Nothing ->
            (model, Cmd.none)

        Just idx ->
            weGotsASpot model idx


{-
getAvailIdxT2B - search for an open spot from top to bottom
-}
getAvailIdxT2B : Int -> Connect4Board -> Maybe Int -> Maybe Int
getAvailIdxT2B idx squares last =
    case Array.get idx squares of
        Nothing ->
            -- idx out of range
            last

        Just maybeVal ->
            -- idx was in range, now we have a maybe to match against
            case maybeVal of
                Nothing ->
                    -- this idx was open, check the next row to see if we have a better option
                    getAvailIdxT2B (idx + cols) squares (Just idx)

                Just val ->
                    -- the square was taken, return last
                    last

{-
select a square and check for a win
-}
weGotsASpot : Model -> Int -> (Model, Cmd Msg)
weGotsASpot model idx =
    let
        newsquares =
            -- set square state at idx, get updated board
            setSquareStateByIdx idx (Just model.currentPlayer) model.squares

        newselections =
            -- record the current selection into the selections history list
            List.append (List.singleton idx) model.selections

        (nextGameState, maybeWinner) =
            -- check for winners and return nextstate,winner tuple
            case checkForWinner newsquares idx model.currentPlayer of
                True ->
                    -- model.currentPlayer is the winner
                    (GameOver, Just model.currentPlayer)

                False ->
                    -- no winner, check if board is full
                    case List.length newselections == rows * cols of
                        True ->
                            -- board full. tie. no winners here today
                            (GameOver, Nothing)

                        False ->
                            -- no winners, not full, play on
                            (model.gameState, Nothing)

        newmodel = {model |
            squares = newsquares
            , selections = newselections
            , currentPlayer = togglePlayer model.currentPlayer
            , winner = maybeWinner
            , gameState = nextGameState
            }
    in
        (newmodel, Cmd.none)


{-
the list of winner check functions
-}
winnerCheckFunctions: List WinnerCheckFunction
winnerCheckFunctions = [
    checkVerticalWin
    ,checkHorizontalWin
    ,checkDiagonalWin
    ]

{-
wrapper to pass the list of checker functions to checkForWinnerCore
-}
checkForWinner : Connect4Board -> Int -> SquareState -> Bool
checkForWinner squares idx expectedstate =
    checkForWinnerCore squares idx expectedstate winnerCheckFunctions

{-
calls the functions in checkerfuns to check for a winner
-}
checkForWinnerCore : Connect4Board -> Int -> SquareState -> List WinnerCheckFunction -> Bool
checkForWinnerCore squares idx expectedstate checkerfuns =
    case checkerfuns of
        [] ->
            -- no more functions to test with
            False

        currentFunction::remainingFunctions ->
            -- test with currentFunction
            case currentFunction squares idx expectedstate of
                True ->
                    -- found winner
                    True

                False ->
                    -- no winner yet. recurse on the rest of the function list
                    checkForWinnerCore squares idx expectedstate remainingFunctions


{-
check for a horizontal win around the given idx
-}
checkHorizontalWin : Connect4Board -> Int -> SquareState -> Bool
checkHorizontalWin board idx expectedstate =
    let
        row =
            (idx // cols)

        minidx =
            -- greater of the first idx of the row, or 3 to our left
            max (row * cols) (idx - 3)

        maxidx =
            -- lesser of the last idx of the row, or 3 to our right
            min (((row+1) * cols) - 1) (idx+3)
    in
        checkRange4Winner board minidx maxidx expectedstate 0



{-
check for a streak on a range. it's the user's responsibility to make sure the range
makes contextual sense. for example, this function does not check if the range
crosses row boundaries. it's up to the caller to do that.
-}
checkRange4Winner : Connect4Board -> Int -> Int -> SquareState -> Int -> Bool
checkRange4Winner board idx maxidx expectedstate streakCount =
    if idx > maxidx then
        -- and you gone too far and you know it don't matter anyway...  you can rely on...
        False
    else
        case getValByIdx idx board of
            Just state ->
                -- a player has already selected this square)
                if state == expectedstate then
                    -- it was the expected player. increment our streak count
                    let
                        newStreakCount = streakCount + 1
                    in
                        if newStreakCount == connectWhat then
                            -- we matched a streak of the expected length
                            True
                        else
                            -- streak in progress. continue searching
                            checkRange4Winner board (idx+1) maxidx expectedstate newStreakCount
                else
                    -- it was the wrong player. reset our counter to 0
                    checkRange4Winner board (idx+1) maxidx expectedstate 0

            Nothing ->
                -- it was an unused cell. reset our counter to 0
                checkRange4Winner board (idx+1) maxidx expectedstate 0


{-
check for a Vertical Win
wrapper for checkVerticalWinCore. passes in initial '0' count
other check functions have their own contextual reasons for requiring a wrapper-
like function. in this case we just fake it for consistency and for the ability
to collect the checker functions into a list.
-}
checkVerticalWin : Connect4Board -> Int -> SquareState -> Bool
checkVerticalWin squarestates idx expectedstate =
    checkVerticalWinCore squarestates idx expectedstate 0


{-
core of the check for a vertical win.
vertical checks can be short circuited at first non-expected cell
-}
checkVerticalWinCore : Connect4Board -> Int -> SquareState -> Int -> Bool
checkVerticalWinCore squarestates idx expectedstate count =
    if count == connectWhat then
        -- we did it. we really did it.
        True
    else
        case getValByIdx idx squarestates of
            Just st ->
                -- cell at 'idx' is selected. check if it's expected player
                if st == expectedstate then
                    -- it was the expected player, increment and keep checking
                    checkVerticalWinCore squarestates (idx + cols) expectedstate (count + 1)
                else
                    -- wrong player and we out.
                    False

            Nothing ->
                -- unselected cell... under our current selection? shldnvrhapn
                False


{-
check the diagonals off of the current idx for a win
-}
checkDiagonalWin : Connect4Board -> Int -> SquareState -> Bool
checkDiagonalWin board idx expectedstate =
    let
        streak1 =
            diagonalPosSlope board idx expectedstate

        streak2 =
            diagonalNegSlope board idx expectedstate
    in
        streak1 >= connectWhat
        || streak2 >= connectWhat


{-
check for a win on the diagonal with positive slope left to right
-}
diagonalPosSlope : Connect4Board -> Int -> SquareState -> Int
diagonalPosSlope board idx expected =
    let
        (row,col) =
            toColRow idx

        downleft =
            diagonalDownLeft board (row+1) (col-1) expected 0

        upright =
            diagonalUpRight board (row-1) (col+1) expected 0
    in
        downleft + upright + 1


{-
check for a win on the diagonal with negative slope left to right
-}
diagonalNegSlope : Connect4Board -> Int -> SquareState -> Int
diagonalNegSlope board idx expected =
    let
        (row,col) =
            toColRow idx

        upleft =
            diagonalUpLeft board (row-1) (col-1) expected 0

        downright =
            diagonalDownRight board (row+1) (col+1) expected 0
    in
        upleft + downright + 1


{-
check for a streak on the diagonal down and to the left
-}
diagonalDownLeft : Connect4Board -> Int -> Int -> SquareState -> Int -> Int
diagonalDownLeft board row col expectedstate count =
    if count == len2Check then
        count
    else if row > (rows - 1) || col < 0 then
        count
    else if getSquareState row col board == Just expectedstate then
        diagonalDownLeft board (row+1) (col-1) expectedstate (count+1)
    else
        count

{-
check for a streak on the diagonal up and to the right
-}
diagonalUpRight : Connect4Board -> Int -> Int -> SquareState -> Int -> Int
diagonalUpRight board row col expectedstate count =
    if count == len2Check then
        count
    else if row < 0 || col > (cols - 1) then
        count
    else if getSquareState row col board == Just expectedstate then
        diagonalUpRight board (row-1) (col+1) expectedstate (count+1)
    else
        count

{-
check for a streak on the diagonal up and to the left
-}
diagonalUpLeft : Connect4Board -> Int -> Int -> SquareState -> Int -> Int
diagonalUpLeft board row col expectedstate count =
    if count == len2Check then
        count
    else if row < 0 || col < 0 then
        count
    else if getSquareState row col board == Just expectedstate then
        diagonalUpLeft board (row-1) (col-1) expectedstate (count+1)
    else
        count

{-
check for a streak on the diagonal down and to the right
-}
diagonalDownRight : Connect4Board -> Int -> Int -> SquareState -> Int -> Int
diagonalDownRight board row col expectedstate count =
    if count == len2Check then
        count
    else if row > (rows - 1) || col > (cols - 1) then
        count
    else if getSquareState row col board == Just expectedstate then
        diagonalDownRight board (row+1) (col+1) expectedstate (count+1)
    else
        count

