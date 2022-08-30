namespace App
open Feliz
open Data


// Sodoku Solver written by Philip Nguyen


// Represents row and column index values
type CellPosition = int * int

type CellValue = int option 

// Candidate list keeps track of a integer array which the cell could possibly still be
type Candidates = int array

// A cell consists of a value, candidate list, and a position
type Cell = (CellValue * Candidates * CellPosition) 


module Sodoku = 

    // Converts char into an integer
    let inline charToInt c = int c - int '0'

    // Parses sodoku string from project euler and turns it into a string array    
    let sodokuData = (Data.projectEulerDataString.Split "\n") |> Array.map(fun a  -> a.Trim()) |> Array.filter(fun a -> a.Length = 9) 

    // Returns the string representation of a specified puzzle
    let getPuzzle i:string = sodokuData.[9 * i .. 9 * i + 8 ] |>  Array.fold (+) ""

    // Takes string and turns it into an array of integers
    let strToArray str =
        Seq.toArray str
        |> Array.filter(fun a -> Array.contains (charToInt a) [|0..9|]   )
        |> Array.map charToInt


    // Makes a puzzle Cell array from specified integer index
    let makeDataTable (i:int):Cell array  = 
            strToArray (getPuzzle i) 
            |> Array.mapi(fun  i a -> 
                            let r = i / 9
                            let c = i % 9
                            if a = 0 then 
                                (None, [|1..9|], (r,c))
                            else
                                (Some(a), [||],(r,c) ))

    
    let getRow (i:int) (table:Cell array): Cell array = table |> Array.filter(fun (_,_,(r,_)) -> r = i)

    let getCol  (i:int) (table:Cell array): Cell array= table |> Array.filter(fun (_,_,(_,c)) -> c = i)
    
    let getNumbers (table:Cell array) : int array = table  |> Array.map(fun (io,_,_) -> io ) |> Array.choose id

    let getRowNumbers (i:int) (table:Cell array) : int array = table |> getRow i |> getNumbers

    let getColNumbers (i:int) (table:Cell array) : int array = table |> getCol i |> getNumbers

    let getColNumbersByCellPosition ((_,c):CellPosition)  (table:Cell array): int array = table |> getCol c |> getNumbers

    let getRowNumbersByCellPosition ((r,_):CellPosition)  (table:Cell array): int array = table |> getRow r |> getNumbers

    let regionOfCell ((r,c):CellPosition):(int * int) = r / 3, c / 3 

    let getCellsInRegion ((r,c):CellPosition) (table:Cell array): Cell array = table |> Array.filter(fun (_,_,(i,j)) -> r = (i / 3) && c = (j / 3))  

    let getNumbersInRegion ((r,c):CellPosition) (table:Cell array):int array = table |> getCellsInRegion (r,c)  |>  getNumbers

    let getNumbersInRegionByCellPosition(cellPosition:CellPosition) (table:Cell array): int array  =   table |> getNumbersInRegion (cellPosition |> regionOfCell) 


    let getBlankCells (table:Cell array):Cell array = table |> Array.filter(fun (a,_,_) -> a.IsNone)

    let getBlankCellPosition (table:Cell array): CellPosition array = table |> getBlankCells |> Array.map(fun (_,_,cp) -> cp)

    // Calculates if puzzle is complete filled in, but not necessarily correct    
    let isCompletelyFilled (table:Cell array) :bool = table |> getBlankCells |> Array.isEmpty   

    let isRowComplete (table:Cell array) (i:int) : bool = (table |> getRowNumbers i |> Array.distinct |> Array.length) = 9  

    let isColComplete (table:Cell array) (i:int) : bool = (table |> getColNumbers i |> Array.distinct |> Array.length) = 9 

    let isRegionComplete  (table:Cell array) (r,c) : bool = (table |> getNumbersInRegion (r,c) |> Array.distinct |> Array.length) = 9 

    let updateCellValue  ((_,_,cp):Cell) (newValue:int) = (Some(newValue),[||],cp) 

    let candidates cell = 
        let (_,l,_) = cell 
        l 

    let cellPosition cell = 
        let (_,_,cp) = cell 
        cp


    // Updates a cell in a table
    let updateTable (oldCell:Cell) (newCell:Cell) (table:Cell array):Cell array = table |> Array.map(fun cell -> if cell = oldCell then newCell else cell)
    

    let tryUpdateCellValue (cell:Cell) (cp2:CellPosition) (newValue:int):Cell = 
        if cellPosition cell = cp2 then 
            updateCellValue cell newValue
        else 
            cell

    // Updates the value of a cell in a table
    let updateTableCellValue (cellPosition:CellPosition) (newValue:int) (table:Cell array)  : Cell array =
        table |> Array.map(fun a -> tryUpdateCellValue a cellPosition newValue)


    let getCellCandidateLength (co:Cell option):int option  =
                                        match co with 
                                        | Some(_,l,_) -> Some(l.Length)
                                        | None -> None

    let easiestCount (table:Cell array): int option = 
            table |> getBlankCells |> Array.filter(fun (a,l,_) -> a.IsNone && (l.Length > 0)) |> Array.sortBy(fun (_,l,_) -> l.Length) |> Array.tryHead |> getCellCandidateLength


    // Gets the cell with the least amount of values in the candidate list.
    let easiestRandomCell (table:Cell array): Cell option = 
        match (easiestCount table) with 
        | Some(c) ->
            table 
            |> getBlankCells
            |> Array.filter( fun (_,l,_) -> l.Length = c)
            |> Array.sortBy(fun _ -> (new System.Random()).Next())
            |> Array.tryHead
        | None -> None

    // Verify all row have 9 distinct values
    let validateAllRows (table:Cell array) = [| 0..8 |]  |> Array.map (table |> isRowComplete) |> Array.reduce (&&)

    // Verify all columns have 9 distinct values
    let validateAllCols (table:Cell array) = [| 0..8 |]  |> Array.map (table |> isColComplete) |> Array.reduce (&&)

    // Verify all partitions have 9 distinct values
    let validateAllPartitions (table:Cell array):bool = 
        (Array.allPairs [|0..2|] [|0..2|])  
        |> Array.map (table |> isRegionComplete)
        |> Array.reduce (&&)

    // Verify sodoku table is completed correctly
    let validateResult (table:Cell array) : bool = 
        validateAllRows table && validateAllCols table && validateAllPartitions table


    // Remove specified integers from candidate list of a cell
    let removeItemsFromCandidateList  ((_,l: Candidates,cp):Cell) (itemsToRemove:int array):Cell = (None,l |> Array.filter(fun a -> (Array.contains a itemsToRemove) = false) ,cp)


    
    let tryUpdateCandidateList (cell:Cell) (cp2:CellPosition) (itemsToRemove:int array):Cell = 
        if cellPosition cell = cp2 then 
            removeItemsFromCandidateList cell itemsToRemove
        else 
            cell

    // Remove items from candidate list in table at specified cell position
    let updateTableCandidateList (cellPosition:CellPosition) (itemsToRemove:int array) (table:Cell array)  : Cell array =
        table |> Array.map(fun a -> tryUpdateCandidateList a cellPosition itemsToRemove)


    // Determines numbers in the same column, row, and region of the cell. These numbers cannot be in the candidate list of the cell, so 
    // the values found can safely be removed from it's candidate list.
    let candidateValuesUpdateForCellPosition (table:Cell array) (cellPosition:CellPosition):Cell array =
        let valuesToRemove = 
            (getRowNumbersByCellPosition cellPosition table) 
            |> Array.append (getColNumbersByCellPosition cellPosition table) 
            |> Array.append (getNumbersInRegionByCellPosition cellPosition table)     
            |> Array.distinct           

        updateTableCandidateList cellPosition valuesToRemove table


    // Refresh candidates for entire table
    let refreshCandidates (table:Cell array): Cell array = Array.fold candidateValuesUpdateForCellPosition table (table |> getBlankCellPosition)


    // For the specified region, find any cell with a unique value in the candidate list. A unique
    // value means that the cell is immediately solvable.
    // For example: A unique value in the a region would be if the number 7 occurs only once among all the 
    // candidate lists in that region. The cell value must be 7.
    let tryFindUniqueInt table (r,c):int option = 
        let itemsInRegion: Cell array = getCellsInRegion  (r,c) table |> Array.filter(fun (a,_,_) -> a.IsNone ) 

        itemsInRegion 
        |> Array.map(fun (_,l: Candidates,_) -> l) 
        |> Array.concat
        |> Array.countBy id
        |> Array.filter(fun (_,cnt) -> cnt = 1)
        |> Array.map fst
        |> Array.tryHead
    

    // Get cell with specified candidate number
    let findCellWithCandidateNumber table (r,c) i: (Cell * int) option =
        getCellsInRegion (r,c) table
        |> Array.filter(fun (_,l,cp) ->  Array.contains  i l)
        |> Array.map(fun a -> (a,i))
        |> Array.tryExactlyOne



    let findUniqueCandidateInRegion (table:Cell array) (r,c):(Cell * int) option =
        match tryFindUniqueInt table (r,c) with 
        | Some(i) -> findCellWithCandidateNumber table (r,c) i     
        | None -> None 

    // Find all unique candidate numbers for each region
    let findAllUnique (table:Cell array): (Cell * int) array = 
        ([|0..2|] ,[|0..2|] )
        ||> Array.allPairs 
        |> Array.map (findUniqueCandidateInRegion table )
        |> Array.choose id


    // Gets all candidate lists for the input Cells
    let getCandidates (lst:Cell array): int array = lst |> Array.map(fun (_,l,_) -> l ) |> Array.concat



    // Gets random puzzle from the Project Euler data set
    let randomPuzzle() = refreshCandidates (makeDataTable (0)) 

    // These puzzles require brute force to solve.
    let hardPuzzleList = [|6;9;27;41;44;46;47;48;49|] 

    // Get random hard puzzle which requires brute force.
    let randomHardPuzzle() =  
        let randomHardIndex = hardPuzzleList.[(new System.Random()).Next(0, hardPuzzleList.Length)]
        refreshCandidates (makeDataTable randomHardIndex)



    // "Line Strategy: Advanced Candidate Removal Strategy"
    // This is an advanced method of removing candidates from the cells of a table
    // If a region has 2 or 3 candidates numbers which form a single row or column, then you can eliminate 
    // that same candidate number from other regions in the same row or column. 
    // Steps
    // 1. Look at a single region
    // 2. Look at the candidate numbers of all unfilled cells to see if it forms a "single file", either row or column.
    // 3. Extend the line that forms onto the other regions, and you can deduce that those cell's candidate numbers cannot 
    //    be the "single file" value you found in step 2. 
    // 4. Repeats this for all regions
    let lineStrategyRow table inputRegion  = 
        let inputRegionRow,_ = inputRegion

        let getRowOfRegionCells (localIndex:int) (region :Cell array)  = 
            let index = (region |> Array.map(fun (_,_,(r,c)) -> r ) |> Array.sortBy id).[localIndex] 
            region |> Array.filter(fun (_,_,(r,c)) -> r = index)

        let currentRegionCells =  table |> getCellsInRegion inputRegion

        currentRegionCells
        |> getCandidates 
        |> Array.countBy id
        |> Array.filter(fun (i, cnt) -> cnt = 3 || cnt = 2)
        |> Array.map(fun (i,cnt) ->
                            (i, 
                            [|0..2 |] 
                            |> Array.filter(fun c -> (currentRegionCells.[3 * c.. 3 * c + 2] |> getCandidates |> Array.filter(fun b -> b = i) |> Array.length) = cnt)
                            |> Array.tryHead))
        |> Array.map(fun (i, rowIndexOption ) ->
                                match rowIndexOption with 
                                | Some(r) -> 
                                        match  (table |> getRow (r + 3 * inputRegionRow) |> Array.tryHead)   with 
                                        | Some(_,_,cp) -> Some(i,fst cp)   
                                        | None -> None 
                                | None -> None) 
        |> Array.choose id           
        |> Array.map(fun (i,rowIndex) -> 
                            (
                            table 
                            |> getRow rowIndex
                            |> Array.filter(fun c -> c |> candidates |> Array.contains i )
                            |> Array.filter(fun b ->   not (Array.contains  b  currentRegionCells  )) 
                            |> Array.map cellPosition
                            ,i))
        |> Array.filter(fun a -> (fst a).Length > 0)


    let lineStrategyCol table inputRegion  = 
        let _,inputRegionCol = inputRegion

        let getColOfRegionCells (localIndex:int) (region :Cell array)  = 
            let index = (region |> Array.map(fun (_,_,(r,c)) -> c ) |> Array.sortBy id).[localIndex] 
            region |> Array.filter(fun (_,_,(r,c)) -> c = index)

        let currentRegionCells =  table |> getCellsInRegion inputRegion

        let mapColIndexToCellIndexList (i:int): int array = 
            match i with 
            | 0 -> [| 0 ; 3 ; 6 |]
            | 1 -> [| 1 ; 4 ; 7 |]
            | 2 -> [| 2 ; 5 ; 8 |]
            | _ -> [||]

        currentRegionCells
        |> getCandidates 
        |> Array.countBy id
        |> Array.filter(fun (i, cnt) -> cnt = 3 || cnt = 2)
        |> Array.map(fun (i,cnt) ->
                            (i, 
                            [|0..2 |]                       
                            |> Array.filter(fun c -> mapColIndexToCellIndexList c |> Array.map( fun a -> currentRegionCells.[a])  |> getCandidates |> Array.filter(fun b -> b = i) |> Array.length = cnt)
                            |> Array.tryHead))
        |> Array.map(fun (i, colIndexOption ) ->
                                match colIndexOption with 
                                | Some(c) -> 
                                        match  (table |> getCol (c + 3 * inputRegionCol) |> Array.tryHead)   with 
                                        | Some(_,_,cp) -> Some(i,snd cp)   
                                        | None -> None 
                                | None -> None) 
        |> Array.choose id           
        |> Array.map(fun (i,colIndex) -> 
                            (
                            table 
                            |> getCol colIndex
                            |> Array.filter(fun c -> c |> candidates |> Array.contains i )
                            |> Array.filter(fun b ->   not (Array.contains  b  currentRegionCells  )) 
                            |> Array.map cellPosition,
                            i))
        |> Array.filter(fun a -> (fst a).Length > 0)
   

    let lineStrategyRowEntireTable inputTable = 
        (Array.allPairs [|0..2|] [|0..2|])  
        |> Array.map (lineStrategyRow inputTable )
        |> Array.concat
        |> Array.map(fun (cpl,i) ->  (cpl |> Array.map(fun c -> (c,i))) )
        |> Array.concat
        |> Array.fold (fun tbl (cp,i) ->  updateTableCandidateList cp [|i|]  tbl)  inputTable  


    let lineStrategyColEntireTable inputTable = 
        (Array.allPairs [|0..2|] [|0..2|])
        |> Array.map (lineStrategyCol inputTable)
        |> Array.concat
        |> Array.map(fun (cpl,i) ->  (cpl |> Array.map(fun c -> (c,i))) )
        |> Array.concat
        |> Array.fold (fun tbl (cp,i) ->  updateTableCandidateList cp [|i|]  tbl)  inputTable  
    

    // One step will attempt to fill in any values using deductive reasoning.
    // If brute force is enabled, then guessing will occur if no logical choice can be made.
    let step (table:Cell array) (bruteForce:bool):Cell array = 


            // Find mininum count of candidates among all the cells in the table. 
            let easiestCount = 
                match (easiestCount table ) with 
                | Some(i) -> i 
                | None -> 0

            let uniqueList = findAllUnique table |> Array.tryHead 

            if easiestCount = 1 then // If there exists a cell with only a single candidate item, then fill that cell in                
                match (easiestRandomCell  table) with 
                | Some (_,l,cp) -> table |> updateTableCellValue cp  l.[0] |> refreshCandidates
                | None -> table
            elif uniqueList.IsSome then // If there is a candidate number where only one copy of that candidate number exists in the region, that cell is solvable
                let (cell,i) = uniqueList.Value
                (updateTable cell (updateCellValue cell i) table ) 
                |> refreshCandidates 
            else 
                // Remove Candidates numbers based on the "Line" Strategy
                let newTable = refreshCandidates (lineStrategyColEntireTable (lineStrategyRowEntireTable table))

                // Brute force works by finding the cell with the least amount of candidate items. Then randomly fill in a cell that is, or tied for the least amount of candidate items.
                if bruteForce && newTable = table then 
                    match (easiestRandomCell newTable)  with  
                    | Some(c) -> 
                        let (_,l,cp) = c 
                        let rnd = l |> Array.sortBy(fun a -> (new System.Random()).Next()) |> Array.tryHead 
                        
                        match rnd with 
                        | Some(i) ->   
                            let newPosibilityList = l |> Array.filter(fun a -> a <> i)
                            let newCell = (Some(i),newPosibilityList,cp )
                            refreshCandidates (updateTable c  newCell newTable )
                        | None -> newTable
                    | None -> newTable
                else 
                    newTable


    // Try solving the puzzle by looping through the step function. 
    // If guessing is involved, the puzzle may end up in an unsolvable state.
    let attemptSolve (table:Cell array) (guess:bool):Cell array = 
        (table,0) |>
        Array.unfold(fun (tbl,i) -> 
            if validateResult tbl || i > 60 then 
                None
            else
                let newTable = step tbl guess
                Some(newTable , (newTable,i + 1) ) 
        ) 
        |> Array.last


    //Infinitely attempt to solve the puzzle. Return the first valid puzzle position, either with bruce force or only using deductive reasoning
    let solve (table:Cell array) (bruteForce:bool):Cell array = 
        if bruteForce then             
            Seq.initInfinite(fun a -> a) 
            |> Seq.map(fun  _ -> attemptSolve table true)
            |> Seq.filter validateResult
            |> Seq.take 1
            |> Seq.exactlyOne
        else 
            attemptSolve table false
            
open Sodoku



type Components =

    [<ReactComponent>]
    static member HelloWorld() =        


        let cellHtml (cell:Cell) =                              
            let (cellValue,Candidates,_)  = cell
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
                            prop.children (Candidates |> Array.map(fun a ->  Html.text a))
                        ]
                ]
            ]

        let ((tableState: Cell array),setTableState) = React.useState( refreshCandidates(randomPuzzle()))

        let rowHtml (table:Cell array) i =  table |> getRow i |> Array.map cellHtml

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
            Html.a [
                prop.style [ style.textAlign.center ]
                prop.href "https://docs.google.com/presentation/d/1glBa2K1iu_BEqa9e6qSZvsVue4HtHAGMMYjOixmKQB4/edit?usp=sharing"
                           
                prop.children [
                    Html.h4 [
                        prop.text "Slides"
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
                prop.children [Html.tbody ([|0..8|] |>  Array.map(fun a -> Html.tr (rowHtml tableState a)))]
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
                    if (isCompletelyFilled tableState) = false then
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
            Html.h5 [
                prop.text "Relation: Philip's Director at EnerSys"
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

