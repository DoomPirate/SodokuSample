namespace App

open Feliz



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


    // Raw data straight from Project Euler website
    // https://projecteuler.net/problem=96
    let projectEulerDataString = 
        "Grid 01
        003020600
        900305001
        001806400
        008102900
        700000008
        006708200
        002609500
        800203009
        005010300
        Grid 02
        200080300
        060070084
        030500209
        000105408
        000000000
        402706000
        301007040
        720040060
        004010003
        Grid 03
        000000907
        000420180
        000705026
        100904000
        050000040
        000507009
        920108000
        034059000
        507000000
        Grid 04
        030050040
        008010500
        460000012
        070502080
        000603000
        040109030
        250000098
        001020600
        080060020
        Grid 05
        020810740
        700003100
        090002805
        009040087
        400208003
        160030200
        302700060
        005600008
        076051090
        Grid 06
        100920000
        524010000
        000000070
        050008102
        000000000
        402700090
        060000000
        000030945
        000071006
        Grid 07
        043080250
        600000000
        000001094
        900004070
        000608000
        010200003
        820500000
        000000005
        034090710
        Grid 08
        480006902
        002008001
        900370060
        840010200
        003704100
        001060049
        020085007
        700900600
        609200018
        Grid 09
        000900002
        050123400
        030000160
        908000000
        070000090
        000000205
        091000050
        007439020
        400007000
        Grid 10
        001900003
        900700160
        030005007
        050000009
        004302600
        200000070
        600100030
        042007006
        500006800
        Grid 11
        000125400
        008400000
        420800000
        030000095
        060902010
        510000060
        000003049
        000007200
        001298000
        Grid 12
        062340750
        100005600
        570000040
        000094800
        400000006
        005830000
        030000091
        006400007
        059083260
        Grid 13
        300000000
        005009000
        200504000
        020000700
        160000058
        704310600
        000890100
        000067080
        000005437
        Grid 14
        630000000
        000500008
        005674000
        000020000
        003401020
        000000345
        000007004
        080300902
        947100080
        Grid 15
        000020040
        008035000
        000070602
        031046970
        200000000
        000501203
        049000730
        000000010
        800004000
        Grid 16
        361025900
        080960010
        400000057
        008000471
        000603000
        259000800
        740000005
        020018060
        005470329
        Grid 17
        050807020
        600010090
        702540006
        070020301
        504000908
        103080070
        900076205
        060090003
        080103040
        Grid 18
        080005000
        000003457
        000070809
        060400903
        007010500
        408007020
        901020000
        842300000
        000100080
        Grid 19
        003502900
        000040000
        106000305
        900251008
        070408030
        800763001
        308000104
        000020000
        005104800
        Grid 20
        000000000
        009805100
        051907420
        290401065
        000000000
        140508093
        026709580
        005103600
        000000000
        Grid 21
        020030090
        000907000
        900208005
        004806500
        607000208
        003102900
        800605007
        000309000
        030020050
        Grid 22
        005000006
        070009020
        000500107
        804150000
        000803000
        000092805
        907006000
        030400010
        200000600
        Grid 23
        040000050
        001943600
        009000300
        600050002
        103000506
        800020007
        005000200
        002436700
        030000040
        Grid 24
        004000000
        000030002
        390700080
        400009001
        209801307
        600200008
        010008053
        900040000
        000000800
        Grid 25
        360020089
        000361000
        000000000
        803000602
        400603007
        607000108
        000000000
        000418000
        970030014
        Grid 26
        500400060
        009000800
        640020000
        000001008
        208000501
        700500000
        000090084
        003000600
        060003002
        Grid 27
        007256400
        400000005
        010030060
        000508000
        008060200
        000107000
        030070090
        200000004
        006312700
        Grid 28
        000000000
        079050180
        800000007
        007306800
        450708096
        003502700
        700000005
        016030420
        000000000
        Grid 29
        030000080
        009000500
        007509200
        700105008
        020090030
        900402001
        004207100
        002000800
        070000090
        Grid 30
        200170603
        050000100
        000006079
        000040700
        000801000
        009050000
        310400000
        005000060
        906037002
        Grid 31
        000000080
        800701040
        040020030
        374000900
        000030000
        005000321
        010060050
        050802006
        080000000
        Grid 32
        000000085
        000210009
        960080100
        500800016
        000000000
        890006007
        009070052
        300054000
        480000000
        Grid 33
        608070502
        050608070
        002000300
        500090006
        040302050
        800050003
        005000200
        010704090
        409060701
        Grid 34
        050010040
        107000602
        000905000
        208030501
        040070020
        901080406
        000401000
        304000709
        020060010
        Grid 35
        053000790
        009753400
        100000002
        090080010
        000907000
        080030070
        500000003
        007641200
        061000940
        Grid 36
        006080300
        049070250
        000405000
        600317004
        007000800
        100826009
        000702000
        075040190
        003090600
        Grid 37
        005080700
        700204005
        320000084
        060105040
        008000500
        070803010
        450000091
        600508007
        003010600
        Grid 38
        000900800
        128006400
        070800060
        800430007
        500000009
        600079008
        090004010
        003600284
        001007000
        Grid 39
        000080000
        270000054
        095000810
        009806400
        020403060
        006905100
        017000620
        460000038
        000090000
        Grid 40
        000602000
        400050001
        085010620
        038206710
        000000000
        019407350
        026040530
        900020007
        000809000
        Grid 41
        000900002
        050123400
        030000160
        908000000
        070000090
        000000205
        091000050
        007439020
        400007000
        Grid 42
        380000000
        000400785
        009020300
        060090000
        800302009
        000040070
        001070500
        495006000
        000000092
        Grid 43
        000158000
        002060800
        030000040
        027030510
        000000000
        046080790
        050000080
        004070100
        000325000
        Grid 44
        010500200
        900001000
        002008030
        500030007
        008000500
        600080004
        040100700
        000700006
        003004050
        Grid 45
        080000040
        000469000
        400000007
        005904600
        070608030
        008502100
        900000005
        000781000
        060000010
        Grid 46
        904200007
        010000000
        000706500
        000800090
        020904060
        040002000
        001607000
        000000030
        300005702
        Grid 47
        000700800
        006000031
        040002000
        024070000
        010030080
        000060290
        000800070
        860000500
        002006000
        Grid 48
        001007090
        590080001
        030000080
        000005800
        050060020
        004100000
        080000030
        100020079
        020700400
        Grid 49
        000003017
        015009008
        060000000
        100007000
        009000200
        000500004
        000000020
        500600340
        340200000
        Grid 50
        300200000
        000107000
        706030500
        070009080
        900020004
        010800050
        009040301
        000702000
        000008006"

    // Converts char into an integer
    let inline charToInt c = int c - int '0'

    // Parses sodoku string from project euler and turns it into a string array    
    let sodokuData = (projectEulerDataString.Split "\n") |> Array.map(fun a  -> a.Trim()) |> Array.filter(fun a -> a.Length = 9) 

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
    // Note, fix something
    let possibleValues (table:Table) (cellPosition:CellPosition) : int array = 
        let values = 
                            [|
                                (getRowNumbersByCellPosition table  cellPosition)
                                (getColNumbersByCellPosition table  cellPosition)
                                (getNumbersInRegionByCellPosition table  cellPosition)
                            |]  |> Array.concat |> Array.distinct        

        [| 1 .. 9|] |> Array.filter(fun a -> (Array.contains  a values) = false )



    // Returns an array of cell positions that do not have a value entered. 
    let unfilledPosition (table:Table) : CellPosition array = 
        table 
        |> Array.mapi(fun r (row:Row) -> 
                                            row 
                                            |> Array.mapi(fun c (cell,_,_) -> 
                                                                                    match cell with 
                                                                                    | None -> Some(r,c)
                                                                                    | Some(_) -> None)  
                                            |> Array.choose id      
        ) |> Array.concat



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
    // Running step one will fill in one entry
    // Fills one at most one value
    // First uses deductive reasoning to determine what to fill in first.
    // If guess is enabled, it will make a guess when no logical choice can be made.
    let step (inputTable:Table) (guess:bool):Table = 


            // Remove values from possibility lists
            let table = refreshPossibilities(inputTable)


            // For the specified region, find any cell with a unique value in the possibility list
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

            
            // Loops through all regions and finds cell that have unique possiblity value in that region
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


                // This section is uses the following strategy
                // 1. Look at a single region
                // 2. Look at the possibility numbers all the unfilled cells to see if it forms a "single file", either row or column.
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
                prop.style [ style.textAlign.center]
            ]
            Html.h3 [
                prop.text "Written in F# by Philip Nguyen"
                prop.style [ style.textAlign.center]
            ]
            Html.a [
                prop.style [ style.textAlign.center]
                prop.href "https://github.com/DoomPirate/SodokuSample"
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

