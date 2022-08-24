namespace App

open Feliz
open Data


// Represents row and column index values
type CellPosition = int * int

type CellValue = int option 

// Possible list keeps track of a integer list which the cell could possibly still be
type PossibleList = int list

// A cell consists of a value, possible list, and a position
type Cell = (CellValue * PossibleList * CellPosition) 

// Represents a row in the sodoku puzzle
type Row = Cell array

// Represents the soduku puzzle 
type Table = Row array 


module Utilities = 

    // Converts char into an integer
    let inline charToInt c = int c - int '0'

    // Parses sodoku string from project euler and turns it into a string array    
    let sodokuData = (Data.projectEulerDataString.Split "\n") |> Array.map(fun a  -> a.Trim()) |> Array.filter(fun a -> a.Length = 9) 

    // Returns the string representation of a specified puzzle
    let getPuzzle i = sodokuData.[9 * i .. 9 * i + 8 ] |>  Array.fold (+) ""

    // Takes string and turns it into an array of integers
    let strToArray str =
        Seq.toArray str
        |> Array.filter(fun a -> List.contains (charToInt a) [0..9]   )
        |> Array.map charToInt

    // Takes a string and turns it into a Table
    let strToTable (str:string):Table =
        let arr =  strToArray str 

        let createRow (r:int):Row  =  
            arr.[9 * r.. 9 * r + 8] 
            |> Array.mapi(fun c num -> 
                                        if num = 0 then 
                                            (None, [1..9], (r,c) )
                                        else
                                            (Some(num), [],(r,c) )
                                    )

        [| 0..9 |] |> Array.map createRow 
        
    

    // Get cell values of specified row index
    let getRowNumbers (table:Table) (i:int) : int array =
        table.[i] 
        |> Array.map(fun  (b,_,_) -> b)
        |> Array.choose id
        

    // Get cell values of specified column index
    let getColNumbers (table:Table) c : int array = 
        [| 0 .. 8|]
        |> Array.map(fun r -> 
                            let (cell,_,_) = table.[r].[c]
                            cell)
        |> Array.choose id                

        

    // Get the numbers in the same column as the specified cell position 
    let getColNumbersByCellPosition (table:Table) (cellPosition:CellPosition) : int array = getColNumbers table (snd cellPosition)



    // Get the numbers in the same row as the specified cell position 
    let getRowNumbersByCellPosition (table:Table) (cellPosition:CellPosition) : int array = getRowNumbers table (fst cellPosition)


    // Retrieves the numbers of specified region
    // Input regions : example 
    // 0, 0 = Upper left square of 9 cells. 
    // 1, 1 = Center 9 cells
    // 2, 2 = Bottom right 9 cells
    let getNumbersInRegion (table:Table) (r:int) (c:int): int array =  
        table.[3 * r.. 3 *  r + 2] 
        |> Array.map(fun d -> d.[3 *  c..3 * c + 2] ) 
        |> Array.concat 
        |> Array.map(fun (b,_,_) -> b) 
        |> Array.choose id
        
 
    // Retrieves the numbers of specified region from specified cell position
    let getNumbersInRegionByCellPosition(table:Table) (cellPosition:CellPosition) : int array = 
        let (rowNumber,columnNumber) = cellPosition
        
        let rowRegion = rowNumber / 3
        
        let columnRegion = columnNumber / 3

        getNumbersInRegion  table rowRegion columnRegion


    // Get numbers which the cell could possibly be.
    // Uses process of elimination by finding numbers in the same column, same row, and same region, 
    // and eliminating them as candidates 
    let possibleValues (table:Table) (cellPosition:CellPosition) : int array = 
        let values = 
            [|
                (getRowNumbersByCellPosition table  cellPosition)
                (getColNumbersByCellPosition table  cellPosition)
                (getNumbersInRegionByCellPosition table  cellPosition)
            |]  |> Array.concat |> Array.distinct        

        [| 1 .. 9|] |> Array.filter(fun a -> (Array.contains  a values) = false )


    // Gets an array of all the cells in the table
    let getAllCells table : Cell array =
        table |> Array.concat

    // Returns an array of cell positions that do not have a value entered. 
    let unfilledPosition (table:Table) : CellPosition array = 
        getAllCells table 
        |> Array.map(fun (cellValue,_,cp) ->       
                            match cellValue with 
                            | Some(_) -> None 
                            | None -> Some(cp)
        ) 
        |> Array.choose id

    // Returns cell from input cell position
    let getCellByCellPosition (table:Table) (cellPosition:CellPosition): Cell = table.[fst cellPosition].[snd cellPosition] 
    


    // Returns cells from input region
    let getCellsByRegion (table:Table) (r:int) (c:int) : Cell array = 
        table.[3 * r.. 3 *  r + 2] 
        |> Array.map(fun d -> d.[3 *  c..3 * c + 2] ) 
        |> Array.concat
        

    // Returns an updated Table where the specified numbers are removed from the possibility list of the specified cell position 
    let removePossibilities (table) (cellPosition:CellPosition) (numbers:int array) : Table = 
            table
            |> Array.mapi(fun rowIndex row -> 
                                row |> Array.mapi(fun colIndex (cellValue,possibilitiesList,cp) -> 
                                                                                                    if cellPosition = (rowIndex,colIndex) then
                                                                                                        // Uodates possibilities list by filtering out the specified numbers
                                                                                                        (cellValue,possibilitiesList  |> List.filter(fun i -> not (List.contains i (Array.toList numbers )))  ,cp)
                                                                                                    else 
                                                                                                        (cellValue,possibilitiesList,cp)
                                                                                        )
                                                )

    // For each unfilled position, calculate the number of possible values
    let unfilledPositionCount  (table:Table) : (CellPosition * int) array = 
        (unfilledPosition table ) |> Array.map(fun a -> (a, (possibleValues table a).Length)  )



    // Gets the cell with the least amount of possibilities
    let easiestCell (table:Table) : (CellPosition * int) = 

        let easiestVal = (unfilledPositionCount table) |> Array.map(fun (cp,i) -> i) |> Array.min
            
        (unfilledPositionCount table) 
        |> Array.filter( fun (cp,i) -> i = easiestVal)
        |> Array.sortBy(fun _ -> (new System.Random()).Next())
        |> Array.head




    // Calculates if puzzle is complete filled in, but not necessarily correct    
    let isSolved (table:Table) = (unfilledPositionCount table).Length = 0


    // Returns the table with an updated cell value
    let updateTable (table:Table) (cellPosition:CellPosition) (newValue:int) : Table =
        table 
        |> Array.mapi(fun rowIndex row -> 
                                row |> Array.mapi(fun colIndex (cellValue,l,cp) -> 
                                                                        if cellPosition = (rowIndex,colIndex) then 
                                                                            (Some(newValue),[],cp)
                                                                        else 
                                                                            (cellValue,l,cp)
                                                            )
        )


    // Returns if a row is complete (having 9 distinct values from 1 to 9)
    let isRowComplete (table:Table) (i:int): bool = ((getRowNumbers table i) |> Array.distinct).Length = 9  


    // Returns if a column is complete (having 9 distinct values from 1 to 9)
    let isColComplete (table:Table) (i:int): bool = ((getColNumbers table i) |> Array.distinct).Length = 9  

    // Returns if a region is complete (having 9 distinct values from 1 to 9)
    let isRegionComplete (table:Table) (r:int) (c:int): bool = ((getNumbersInRegion table r c) |> Array.distinct).Length = 9  


    // Returns if the puzzle is completed and valid
    // Checks rows, columns and regions for validity
    let validateResult (table:Table) : bool = 

        // Loop through each row and determines validity    
        let rowsValid = 
            ([| 0..8 |]
            |> Array.map(fun a ->  isRowComplete table a)
            |> Array.filter(fun a -> a = false)).Length = 0

        // Loop through each column and determines validity      
        let colsValid = 
            ([| 0..8 |]
            |> Array.map(fun a ->  isColComplete table a)
            |> Array.filter(fun a -> a = false)).Length = 0


        // Loop through each region and determines validity       
        let partitionsValid = 
            ((Array.allPairs [|0..2|] [|0..2|])  
            |> Array.map(fun (r,c) -> isRegionComplete table r c) 
            |> Array.filter(fun a -> a = false)).Length = 0


        rowsValid && colsValid && partitionsValid  


    let updateValuesInTable (table:Table) ((newValue,cellPosition): int  * CellPosition) : Table = 
        updateTable table cellPosition newValue


    // Determines if any possibilities can be removed from a cell
    let refreshPossibilities (inputTableState) = 
        let emptyPositionArray = unfilledPosition inputTableState

        let updatePossibilities (table:Table) (cellPosition:CellPosition) =

            let valuesToRemove = 
                                        (getRowNumbersByCellPosition table  cellPosition) 
                                        |> Array.append (getColNumbersByCellPosition table  cellPosition) 
                                        |> Array.append (getNumbersInRegionByCellPosition table  cellPosition)     
                                        |> Array.distinct           

            removePossibilities table cellPosition valuesToRemove
    
        Array.fold updatePossibilities inputTableState emptyPositionArray


    // Main algorithm used to solve the sodoku puzzle
    // One step will either fill in one entry or update the possibility lists
    // First uses deductive reasoning to determine what to fill in first.
    // If guess is enabled, it will make a guess when no logical choice can be made.
    let step (inputTable:Table) (guess:bool):Table = 

            // Updates the possibility lists to see if any numbers can be removed from their respective list.
            let table = refreshPossibilities(inputTable)


            // For the specified region, find any cell with a unique value in the possibility list. A unique
            // means that the cell is immediately solvable, because the cell value hat unique number.
            // For example: A unique value in the a region would be if the number 7 occurs only once among all the 
            // possibility lists in that region. The cell value must be 7.
            let findUniquePossibilityInRegion r c = 

                let itemsInRegion = getCellsByRegion table r c


                let itemsToFillIn = 
                    itemsInRegion 
                    |> Array.map(fun (_,l,_) -> l) 
                    |> List.concat
                    |> Seq.countBy id 
                    |> Seq.toList 
                    |> List.filter(fun (_,cnt) -> cnt = 1)
                    |> List.map fst
                
                itemsToFillIn 
                |> List.map(fun i ->        
                                    (i,
                                    itemsInRegion
                                    |> Array.filter( fun (_,b,_) -> (List.contains i b))
                                    |> Array.map(fun (_,_,d) -> d)
                                    |> Array.head
                                    ))

            
            // Loops through all regions and finds cells that have unique possiblity value in that region
            let cellsToUpdate = 
                ([0..2] ,[0..2] )
                ||> List.allPairs 
                |> List.map(fun (r,c) ->   findUniquePossibilityInRegion r c)
                |> List.concat
                

            // Get the list of items with only one possible item in it
            let single = 
                    table 
                    |> Array.map(fun row -> 
                                        row 
                                        |> Array.map(fun (_,l,cp) ->  (l , cp) )
                                        |> Array.filter(fun (l,_) -> l.Length = 1 )
                    ) 
                    |> Array.concat
                    |> Array.map(fun (i,j) -> (i.Head,j) )
                    |> List.ofArray

            let totalItems = single @ cellsToUpdate

            if totalItems.Length > 0 then  
                // Return an updated table and refresh the possibility lists                                 
                refreshPossibilities(updateValuesInTable table (List.head totalItems) )
            else 

                let emptyPositionArray = unfilledPosition table

                let regions = 
                    ([0..2] ,[0..2] )
                    ||> List.allPairs 


                // This section uses the following strategy
                // 1. Look at a single region
                // 2. Look at the possibility numbers of all unfilled cells to see if it forms a "single file", either row or column.
                // 3. Extend the line that forms onto the other regions, and you can deduce that those cell's possibility numbers cannot 
                //    be the "single file" value you found in step 2. 
                // 4. Repeats this for all regions

                // For each region, find the possibility numbers that exists in a single row

                // Output is in the form of (region row index, region column index, possibility number to delete, row index)
                let getSingleFileRows = 
                            regions 
                            |> List.map(fun (r,c) -> 
                                                let cells = getCellsByRegion table r c  
                                                let combined = cells |> Array.map(fun (_,a,_) -> a) |> Array.fold (@) []
                                                
                                                // Only check numbers that have count 2 or 3, because those are needed to form a "single file"
                                                let possibilitiesToCheck = combined |> List.countBy(fun a -> a) |> List.filter(fun (_,cnt) -> cnt = 2 || cnt = 3) |> List.map fst

                                                // For each possible possiblity number, check to see if it forms a "single file" row
                                                possibilitiesToCheck 
                                                |> List.map(fun n -> 
                                                                    
                                                                    let possibleInARow = 
                                                                        cells 
                                                                        |> Array.filter(fun (_,l,_d) -> List.contains n l) 
                                                                        |> Array.map(fun (_,_,d: CellPosition) -> d) 

                                                                    let checkInRow x  = (x |> Array.map(fun (r,_) -> r) |> Array.distinct |> Array.length) = 1
                                                                    if (possibleInARow |> checkInRow) then 
                                                                        Some(n, fst possibleInARow.[0])
                                                                    else 
                                                                        None
                                                )
                                                |> List.choose id
                                                |> List.map(fun (n, idx) -> (r,c,n,idx))            
                            )
                            |> List.fold (@) []

                // Output is in the form of (region row index, region column index, possibility number to delete, column index)
                let getSingleFileCols = 
                    regions 
                    |> List.map(fun (r,c) -> 
                                        let cells = getCellsByRegion table r c  
                                        let combined = cells |> Array.map(fun (_,a,_) -> a) |> Array.fold (@) []

                                        // Only check numbers that have count 2 or 3, because those are needed to form a "single file"
                                        let possibilitiesToCheck = combined |> List.countBy(fun a -> a) |> List.filter(fun (_,cnt) -> cnt = 2 || cnt = 3) |> List.map fst

                                        // For each possible possiblity number, check to see if it forms a "single file" column
                                        possibilitiesToCheck 
                                        |> List.map(fun n -> 
                                                            let possibleInACol = 
                                                                cells 
                                                                |> Array.filter(fun (_,l,_) -> List.contains n l) 
                                                                |> Array.map(fun (_,_,d) -> d) 
                                                            let checkInCol x  = (x |> Array.map(fun (_,c) -> c) |> Array.distinct |> Array.length) = 1
                                                            if (possibleInACol |> checkInCol) then 
                                                                Some(n, snd possibleInACol.[0])
                                                            else 
                                                                None
                                        )
                                        |> List.choose id
                                        |> List.map(fun (n, idx) -> (r,c,n,idx))

                                        )
                    |> List.fold (@) []


                // Find possibility numbers to delete in the same row, but not in the same region 
                let cellsToDeleteRow = 
                    getSingleFileRows 
                    |> List.map(fun (r,c,n,idx) ->
                                        let row = table.[idx]
                                        row 
                                        |> Array.filter(fun (_,l,_) -> List.contains  n l)  
                                        |> Array.filter(fun (_,_,cp) ->   (fst cp) / 3 <> r || (snd cp) / 3 <> c)
                                        |> Array.map(fun (_,_,cp) -> (cp, n) )
                                        |> List.ofArray
                                        

                    )


                // Find possibility numbers to delete in the same row, but not in the same region 
                let cellsToDeleteCol = 
                    getSingleFileCols 
                    |> List.map(fun (r,c,n,idx) ->
                                        [| 0 .. 8|] 
                                        |> Array.map(fun a ->  table.[a].[idx])
                                        |> Array.filter(fun (_,l,_) -> List.contains  n l)  
                                        |> Array.filter(fun (_,_,cp) ->   (fst cp) / 3 <> r || (snd cp) / 3 <> c)
                                        |> Array.map(fun (_,_,cp) -> (cp, n) )
                                        |> List.ofArray
                                        

                    )


                // The list of cell posibility numbers that can be deleted (rows and columns)           
                let allRemoval = (cellsToDeleteCol @ cellsToDeleteRow) |> List.concat  

            
                let updatePossibilities (table:Table) (cellPosition:CellPosition) =

                    let valuesToRemove = 
                                        allRemoval
                                        |> List.filter(fun (cp:CellPosition,i:int) -> cp = cellPosition) 
                                        |> List.map snd 
                                        |> Array.ofList 
                                        |> Array.distinct     
             
    

                    removePossibilities table cellPosition valuesToRemove
            
                let newTable = Array.fold updatePossibilities table emptyPositionArray


                // If an item was removed from the possibility list then return the new table
                if allRemoval.Length > 0  then 
                    
                    newTable

                // Guess if brute force is enabled and nothing was removed from the possiblity list    
                elif allRemoval.Length = 0 && guess then 

                    // The algorithm is
                    // Calculate the guess that has the least amount of possiblities
                    // Randomly fill in a number for that cell

                    let (easiest,easiestCount) = (easiestCell newTable)        
                
                    if easiestCount > 0 then 
                        let possible = (possibleValues newTable  easiest)  |> Array.sortBy(fun a -> (new System.Random()).Next())    
                        refreshPossibilities(updateTable newTable easiest possible.[0] )
                    else 
                        refreshPossibilities(newTable)
                else 
                    newTable

           

    // Try solving the puzzle by looping through the step function. 
    // If guessing is involved, the puzzle may end up in an unsolvable state.
    let attemptSolve t guess = 
        (t,0) |>
        Array.unfold(fun (tbl,i) -> 
            if isSolved tbl || i > 80 then 
                None
            else
                let newTable = step tbl guess

                Some(newTable , (newTable,i + 1) ) 
        ) 
        |> Array.last



    // Solve the puzzle, either with bruce force or only using deductive reasoning
    let solve (table:Table) (bruteForce:bool):Table = 

        // Infinitely attempt to solve the puzzle. Return the first valid puzzle position
        if bruteForce = true then             
            Seq.initInfinite(fun a -> a) 
            |> Seq.map(fun  _ -> attemptSolve table bruteForce)
            |> Seq.filter isSolved
            |> Seq.take 1
            |> Seq.toList
            |> List.head

        else 
            // Solve the puzzle without brute forcing
            attemptSolve table bruteForce
            
            
    // Gets random puzzle from the project euler data set
    let randomPuzzle() = refreshPossibilities (strToTable (getPuzzle ((new System.Random()).Next(0, 49)) )   ) 

    // Hard puzzles that will require brute force to full solve.
    let hardPuzzleList = [6;9;27;41;44;46;47;48;49] 

    // Gets random hard puzzle
    let randomHardPuzzle() =  
        let randomHardIndex = hardPuzzleList.[(new System.Random()).Next(0, hardPuzzleList.Length)]
        refreshPossibilities (strToTable (getPuzzle randomHardIndex )   )

open Utilities



type Components =

    [<ReactComponent>]
    static member HelloWorld() =        

        let ((tableState),setTableState) = React.useState( randomPuzzle())
          
        Html.div [
         
            Html.h1 [
                prop.text "Sodoku Solver"
                prop.style [ style.textAlign.center ]
            ]
            Html.h3 [
                prop.text "Written in F# by Philip Nguyen"
                prop.style [ style.textAlign.center ]
            ]
            Html.a [
                prop.style [ style.textAlign.center ]
                prop.href "https://github.com/DoomPirate/SodokuSample/blob/main/src/Components.fs"
                prop.children [
                        Html.h4 [
                            prop.text "Source Code (Github)"
                            prop.style [ style.textAlign.center]
                        ]
                ]
            ]
            Html.table [
                prop.id "sodoku"
                prop.style [ 
                    style.marginLeft length.auto
                    style.marginRight length.auto
                ]
                prop.children [
                    Html.tbody ( 
                        tableState
                        |> Array.map(fun b ->
                            Html.tr (
                                    b  
                                    |> Array.map(fun (cellValue,possibleList,_) -> 
                                                    Html.td [
                                                        prop.style [ style.textAlign.center ]
                                                        prop.children [
                                                            match cellValue with 
                                                            | Some(i) -> 
                                                                Html.text i
                                                            | None ->
                                                                Html.div [
                                                                    prop.style [ 
                                                                        style.fontSize 11
                                                                        style.paddingBottom 50
                                                                        style.color "grey"
                                                                     ]

                                                                    prop.children (possibleList |> List.map(fun a ->  Html.text a))
                                                                    
                                                                ]

                                                        ]
                                                    ]
                                                )
                                                ))
                    )
                ]
            ]

            Html.div [
                prop.style [ 
                    style.marginLeft length.auto
                    style.marginRight length.auto
                    style.width 550
                ]
                
                prop.children [

                
                    Html.button [
                        prop.text "Random"
                        prop.onClick (fun _ -> setTableState(randomPuzzle()))
                    ]

                    Html.button [
                        prop.text "Random (Brute Force Needed)"
                        prop.onClick (fun _ -> setTableState(randomHardPuzzle()))
                    ]

                    if (isSolved tableState) = false then
                        Html.button [
                            prop.text "Step (No Guessing)"
                            prop.onClick (fun _ -> 
                                    setTableState(step tableState false)                                    
                            )
                        ]

                        Html.button [
                            prop.text "Attempt Solve (No Guessing)"
                            prop.onClick (fun _ -> setTableState(solve tableState false))
                        ]

                        Html.button [
                            prop.text "Solve (Brute force when needed)"
                            prop.onClick (fun _ -> setTableState(solve tableState true))
                        ]

            
                    elif validateResult tableState then 
                        Html.span [
                            prop.style [
                                style.fontSize 50
                                style.marginLeft 50 
                                style.verticalAlign.middle
                                
                                ]

                            prop.text "Solved!"
                        ]
                    else 
                        Html.span "Error"

                    ]   
            ]

            Html.h2 [
                prop.text "Reference"
                prop.style [ style.textAlign.center ]

            ]
            Html.h3 [
                prop.text "Khaled Bahei-Eldin"
                prop.style [ style.textAlign.center ]
            ]
            Html.h4 [
                prop.text "Director of Engineering"
                prop.style [ style.textAlign.center ]
            ]   
            Html.h4 [
                prop.text "Rivian"
                prop.style [ style.textAlign.center ]
            ]
            Html.h4 [
                prop.style [ style.textAlign.center ]
                prop.children [
                    Html.a [
                        prop.text "518-253-0008"
                        prop.href "tel:518-253-0008"
                        

                    ]
                ]
            ]
            Html.h4 [
                prop.style [ style.textAlign.center ]
                prop.children [
                    Html.a [
                        prop.text "kbaheieldin@rivian.com"
                        prop.href "mailto:kbaheieldin@rivian.com"
                    ]
                ]
            ]
    ]

