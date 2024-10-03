module SudokuLogic exposing
    ( generateSudoku
    , isSudokuComplete
    , isValid
    , updateCompletedSections
    )

import List.Extra
import Random
import Types exposing (DigitValue(..), SudokuGrid)


generateSudoku : Random.Seed -> ( SudokuGrid, Random.Seed )
generateSudoku seed =
    let
        ( difficulty, seed1 ) =
            Random.step (Random.int 40 55) seed

        emptyGrid =
            List.repeat 9 (List.repeat 9 (Changeable 0))

        ( filledGrid, seed2 ) =
            fillGrid seed1 emptyGrid

        ( puzzle, seed3 ) =
            removeCells difficulty filledGrid seed2
    in
    ( puzzle, seed3 )


fillGrid : Random.Seed -> SudokuGrid -> ( SudokuGrid, Random.Seed )
fillGrid seed grid =
    let
        ( newGrid, _, newSeed ) =
            fillCell 0 0 seed grid
    in
    ( newGrid, newSeed )


fillCell : Int -> Int -> Random.Seed -> SudokuGrid -> ( SudokuGrid, Bool, Random.Seed )
fillCell row col seed grid =
    if row == 9 then
        ( grid, True, seed )

    else if col == 9 then
        fillCell (row + 1) 0 seed grid

    else if getCell row col grid /= Changeable 0 then
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


tryValues : Int -> Int -> List Int -> Random.Seed -> SudokuGrid -> ( SudokuGrid, Bool, Random.Seed )
tryValues row col values seed grid =
    case values of
        [] ->
            ( grid, False, seed )

        value :: rest ->
            if isValid row col value grid then
                let
                    newGrid =
                        setCell row col (Changeable value) grid

                    ( filledGrid, success, newSeed ) =
                        fillCell row (col + 1) seed newGrid
                in
                if success then
                    ( filledGrid, True, newSeed )

                else
                    tryValues row col rest newSeed grid

            else
                tryValues row col rest seed grid


removeCells : Int -> SudokuGrid -> Random.Seed -> ( SudokuGrid, Random.Seed )
removeCells cellsToRemove grid seed =
    let
        allPositions =
            List.concatMap (\row -> List.map (\col -> ( row, col )) (List.range 0 8)) (List.range 0 8)

        ( shuffledPositions, newSeed ) =
            shuffle allPositions seed

        positionsToRemove =
            List.take cellsToRemove shuffledPositions

        newGrid =
            List.foldl (\( row, col ) g -> setCell row col (Changeable 0) g) grid positionsToRemove
    in
    ( newGrid, newSeed )


isValid : Int -> Int -> Int -> SudokuGrid -> Bool
isValid row col value grid =
    isValidRow row value grid
        && isValidColumn col value grid
        && isValidBox (row // 3 * 3) (col // 3 * 3) value grid


isValidRow : Int -> Int -> SudokuGrid -> Bool
isValidRow row value grid =
    not (List.member (Changeable value) (List.drop row grid |> List.head |> Maybe.withDefault []))


isValidColumn : Int -> Int -> SudokuGrid -> Bool
isValidColumn col value grid =
    not (List.member (Changeable value) (List.map (\row -> getCell row col grid) (List.range 0 8)))


isValidBox : Int -> Int -> Int -> SudokuGrid -> Bool
isValidBox boxRow boxCol value grid =
    not
        (List.member (Changeable value)
            (List.concatMap
                (\r -> List.map (\c -> getCell r c grid) (List.range boxCol (boxCol + 2)))
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


updateCompletedSections : SudokuGrid -> SudokuGrid
updateCompletedSections grid =
    grid
        |> updateCompletedRows
        |> updateCompletedColumns
        |> updateCompletedSquares


updateCompletedRows : SudokuGrid -> SudokuGrid
updateCompletedRows =
    List.map
        (\row ->
            if isRowCompleted row then
                List.map
                    (\value ->
                        case value of
                            Changeable n ->
                                NotChangeable n

                            NotChangeable n ->
                                NotChangeable n
                    )
                    row

            else
                row
        )


updateCompletedColumns : SudokuGrid -> SudokuGrid
updateCompletedColumns grid =
    List.range 0 8
        |> List.foldl
            (\colIndex accGrid ->
                if
                    accGrid
                        |> List.map (\row -> List.drop colIndex row |> List.head |> Maybe.withDefault (Changeable 0))
                        |> isColumnCompleted
                then
                    accGrid
                        |> List.map
                            (List.indexedMap
                                (\cellIndex cell ->
                                    if cellIndex == colIndex then
                                        case cell of
                                            Changeable n ->
                                                NotChangeable n

                                            NotChangeable n ->
                                                NotChangeable n

                                    else
                                        cell
                                )
                            )

                else
                    accGrid
            )
            grid


updateCompletedSquares : SudokuGrid -> SudokuGrid
updateCompletedSquares grid =
    List.range 0 2
        |> List.foldl
            (\rowBlock accGrid ->
                List.range 0 2
                    |> List.foldl
                        (\colBlock innerAccGrid ->
                            let
                                squareValues =
                                    getSquareValues (rowBlock * 3) (colBlock * 3) innerAccGrid
                            in
                            if isSquareCompleted squareValues then
                                updateSquare (rowBlock * 3) (colBlock * 3) innerAccGrid

                            else
                                innerAccGrid
                        )
                        accGrid
            )
            grid


isSudokuComplete : SudokuGrid -> Bool
isSudokuComplete grid =
    List.all (List.all (\cell -> cell /= Changeable 0)) grid



-- Helper functions


getCell : Int -> Int -> SudokuGrid -> DigitValue
getCell row col grid =
    grid
        |> List.drop row
        |> List.head
        |> Maybe.withDefault []
        |> List.drop col
        |> List.head
        |> Maybe.withDefault (Changeable 0)


setCell : Int -> Int -> DigitValue -> SudokuGrid -> SudokuGrid
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


getSquareValues : Int -> Int -> SudokuGrid -> List DigitValue
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


updateSquare : Int -> Int -> SudokuGrid -> SudokuGrid
updateSquare startRow startCol grid =
    List.indexedMap
        (\rowIndex row ->
            if rowIndex >= startRow && rowIndex < startRow + 3 then
                List.indexedMap
                    (\colIndex cell ->
                        if colIndex >= startCol && colIndex < startCol + 3 then
                            case cell of
                                Changeable n ->
                                    NotChangeable n

                                NotChangeable n ->
                                    NotChangeable n

                        else
                            cell
                    )
                    row

            else
                row
        )
        grid


isRowCompleted : List DigitValue -> Bool
isRowCompleted row =
    List.map
        (\value ->
            case value of
                Changeable n ->
                    n

                NotChangeable n ->
                    n
        )
        row
        |> List.sort
        |> (==) (List.range 1 9)


isColumnCompleted : List DigitValue -> Bool
isColumnCompleted column =
    List.map
        (\value ->
            case value of
                Changeable n ->
                    n

                NotChangeable n ->
                    n
        )
        column
        |> List.sort
        |> (==) (List.range 1 9)


isSquareCompleted : List DigitValue -> Bool
isSquareCompleted square =
    List.map
        (\value ->
            case value of
                Changeable n ->
                    n

                NotChangeable n ->
                    n
        )
        square
        |> List.sort
        |> (==) (List.range 1 9)
