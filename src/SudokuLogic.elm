module SudokuLogic exposing
    ( generateSudoku
    , isSudokuComplete
    , isValid
    )

import List.Extra
import Random
import Types exposing (CellStateBackend(..), DigitValueBackend, SudokuGridBackend)


generateSudoku : Random.Seed -> ( SudokuGridBackend, Random.Seed )
generateSudoku seed =
    let
        ( difficulty, seed1 ) =
            Random.step (Random.int 40 55) seed

        emptyGrid =
            List.repeat 9 (List.repeat 9 { cellState = EmptyCell, value = 0 })

        ( filledGrid, seed2 ) =
            fillGrid seed1 emptyGrid

        ( puzzle, seed3 ) =
            removeCells difficulty filledGrid seed2
    in
    ( puzzle, seed3 )


fillGrid : Random.Seed -> SudokuGridBackend -> ( SudokuGridBackend, Random.Seed )
fillGrid seed grid =
    let
        ( newGrid, _, newSeed ) =
            fillCell 0 0 seed grid
    in
    ( newGrid, newSeed )


fillCell : Int -> Int -> Random.Seed -> SudokuGridBackend -> ( SudokuGridBackend, Bool, Random.Seed )
fillCell row col seed grid =
    if row == 9 then
        ( grid, True, seed )

    else if col == 9 then
        fillCell (row + 1) 0 seed grid

    else if (getCell row col grid).cellState /= EmptyCell then
        fillCell row (col + 1) seed grid

    else
        let
            ( shuffledValues, newSeed ) =
                shuffle (List.range 1 9) seed

            ( filledGrid, success, finalSeed ) =
                tryValues row col shuffledValues newSeed grid
        in
        if success then
            ( filledGrid, True, finalSeed )

        else
            ( grid, False, finalSeed )


tryValues : Int -> Int -> List Int -> Random.Seed -> SudokuGridBackend -> ( SudokuGridBackend, Bool, Random.Seed )
tryValues row col values seed grid =
    case values of
        [] ->
            ( grid, False, seed )

        value :: rest ->
            if isValid row col value grid then
                let
                    newGrid =
                        setCell row col { cellState = RevealedCell, value = value } grid

                    ( filledGrid, success, newSeed ) =
                        fillCell row (col + 1) seed newGrid
                in
                if success then
                    ( filledGrid, True, newSeed )

                else
                    tryValues row col rest newSeed grid

            else
                tryValues row col rest seed grid


removeCells : Int -> SudokuGridBackend -> Random.Seed -> ( SudokuGridBackend, Random.Seed )
removeCells cellsToRemove grid seed =
    let
        allPositions =
            List.concatMap (\row -> List.map (\col -> ( row, col )) (List.range 0 8)) (List.range 0 8)

        ( shuffledPositions, newSeed ) =
            shuffle allPositions seed

        positionsToRemove =
            List.take cellsToRemove shuffledPositions

        newGrid =
            List.foldl (\( row, col ) g -> setCell row col { cellState = EmptyCell, value = (getCell row col g).value } g) grid positionsToRemove
    in
    ( newGrid, newSeed )


isValid : Int -> Int -> Int -> SudokuGridBackend -> Bool
isValid row col value grid =
    isValidRow row value grid
        && isValidColumn col value grid
        && isValidBox (row // 3 * 3) (col // 3 * 3) value grid


isValidRow : Int -> Int -> SudokuGridBackend -> Bool
isValidRow row value grid =
    not (List.member value (List.map .value (List.drop row grid |> List.head |> Maybe.withDefault [])))


isValidColumn : Int -> Int -> SudokuGridBackend -> Bool
isValidColumn col value grid =
    not (List.member value (List.map (\row -> (getCell row col grid).value) (List.range 0 8)))


isValidBox : Int -> Int -> Int -> SudokuGridBackend -> Bool
isValidBox boxRow boxCol value grid =
    not
        (List.member value
            (List.concatMap
                (\r -> List.map (\c -> (getCell r c grid).value) (List.range boxCol (boxCol + 2)))
                (List.range boxRow (boxRow + 2))
            )
        )


shuffle : List a -> Random.Seed -> ( List a, Random.Seed )
shuffle list seed =
    let
        ( shuffledList, newSeed ) =
            List.foldl
                (\item ( acc, currentSeed ) ->
                    let
                        ( index, nextSeed ) =
                            Random.step (Random.int 0 (List.length acc)) currentSeed

                        ( before, after ) =
                            List.Extra.splitAt index acc
                    in
                    ( before ++ (item :: after), nextSeed )
                )
                ( [], seed )
                list
    in
    ( shuffledList, newSeed )


isSudokuComplete : SudokuGridBackend -> Bool
isSudokuComplete grid =
    List.all
        (\row ->
            List.all
                (\cell ->
                    case cell.cellState of
                        RevealedCell ->
                            True

                        Guess val ->
                            val == cell.value

                        EmptyCell ->
                            False
                )
                row
        )
        grid



-- Helper functions


getCell : Int -> Int -> SudokuGridBackend -> DigitValueBackend
getCell row col grid =
    grid
        |> List.drop row
        |> List.head
        |> Maybe.andThen (List.drop col >> List.head)
        |> Maybe.withDefault { cellState = EmptyCell, value = 0 }


setCell : Int -> Int -> DigitValueBackend -> SudokuGridBackend -> SudokuGridBackend
setCell row col value grid =
    List.indexedMap
        (\r rowList ->
            if r == row then
                List.indexedMap
                    (\c cellValue ->
                        if c == col then
                            value

                        else
                            cellValue
                    )
                    rowList

            else
                rowList
        )
        grid


getSquareValues : Int -> Int -> SudokuGridBackend -> List DigitValueBackend
getSquareValues startRow startCol grid =
    List.range startRow (startRow + 2)
        |> List.concatMap
            (\row ->
                List.range startCol (startCol + 2)
                    |> List.map
                        (\col ->
                            getCell row col grid
                        )
            )


updateSquare : Int -> Int -> SudokuGridBackend -> SudokuGridBackend
updateSquare startRow startCol grid =
    List.indexedMap
        (\rowIndex row ->
            if rowIndex >= startRow && rowIndex < startRow + 3 then
                List.indexedMap
                    (\colIndex cell ->
                        if colIndex >= startCol && colIndex < startCol + 3 then
                            { cell | cellState = RevealedCell }

                        else
                            cell
                    )
                    row

            else
                row
        )
        grid


isRowCompleted : List DigitValueBackend -> Bool
isRowCompleted row =
    List.all
        (\cell ->
            case cell.cellState of
                RevealedCell ->
                    True

                Guess val ->
                    val == cell.value

                EmptyCell ->
                    False
        )
        row


isColumnCompleted : List DigitValueBackend -> Bool
isColumnCompleted column =
    List.map .value column
        |> List.sort
        |> (==) (List.range 1 9)


isSquareCompleted : List DigitValueBackend -> Bool
isSquareCompleted square =
    List.map .value square
        |> List.sort
        |> (==) (List.range 1 9)
