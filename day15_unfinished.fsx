#time
open System

type Point = {
    Id : string
    Row : int
    Column : int
    Up : Option<int>
    Down : Option<int>
    Left : Option<int>
    Right : Option<int>
    RiskLevel : int
} 

let rawPoints = 
    System.IO.File.ReadLines "./input/input_day15.txt"
    |> Seq.map (fun f ->  
        f.ToCharArray() 
        |> Array.map (fun f -> 
            f 
            |> sprintf "%c" // Convert to string first for cast to work
            |> int))
    |> Array.ofSeq

let maxRowIndex = rawPoints.Length - 1
let maxColumnIndex = rawPoints[0].Length - 1

let positionToOptionalInt (grid : int array array) (rowIndex : int) (columnIndex : int) =
    if rowIndex < 0 then None
    elif rowIndex > maxRowIndex then None
    elif columnIndex < 0 then None
    elif columnIndex > maxColumnIndex then None
    else Some(grid[rowIndex][columnIndex])

let unboxOptionalInt (optionalInt : Option<int>) =
    match optionalInt with 
    | Some(x) -> x
    | None -> raise(ArgumentNullException("Unbox only values that you know that are not null"))

let unboxOptionalIntOrArbitraryLarge (optionalInt: Option<int>) =
    match optionalInt with
    | Some(x) -> x
    | None -> 99

let points = 
    rawPoints
    |> Array.mapi (fun rowIndex row -> 
        row |> Array.mapi (fun columnIndex value -> 
            let up = positionToOptionalInt rawPoints (rowIndex - 1) columnIndex
            let down = positionToOptionalInt rawPoints (rowIndex + 1) columnIndex
            let left = positionToOptionalInt rawPoints rowIndex (columnIndex - 1)
            let right = positionToOptionalInt rawPoints rowIndex (columnIndex + 1)
            {
                Row = rowIndex
                Column = columnIndex
                Up = up
                Down = down
                Left = left
                Right = right
                RiskLevel = value
                Id = sprintf "%d,%d" rowIndex columnIndex
            }
        )
    )

printfn "Advent of Code Day 15"

let chooseLesserRoute (downwardRoute: List<int*int*int>) (rightwardRoute: List<int*int*int>) =
    let downSum = downwardRoute |> List.sumBy (fun (_,_,value) -> value )
    let rightSum = rightwardRoute |> List.sumBy (fun (_,_,value) -> value )
    if downSum <= rightSum
    then downwardRoute
    else rightwardRoute

let getIndexSafePointDetails (x : int) (y : int) =
    if x > maxRowIndex || y > maxColumnIndex
    then (x,y,999)
    else (x,y,(points[x][y]).RiskLevel)

let rec generateDownwardValues (x: int) (y: int) (downwardRoute: List<int*int*int>) (rightwardRoute: List<int*int*int>) (depthOfTest: int) (currentDepth: int) =

    let downwardPoint = getIndexSafePointDetails (x+1) (y)
    let newDownwardRoute = (downwardRoute @ [downwardPoint])
    let righwardPoint = getIndexSafePointDetails (x) (y+1)
    let newRightwardRoute = (rightwardRoute @ [righwardPoint])

    if currentDepth = depthOfTest
    then 
        chooseLesserRoute newDownwardRoute newRightwardRoute
    else 
        let down = generateDownwardValues (x+1) y newDownwardRoute newRightwardRoute depthOfTest (currentDepth + 1)
        let right = generateDownwardValues x (y+1) newDownwardRoute newRightwardRoute  depthOfTest (currentDepth + 1)
        chooseLesserRoute down right

let rec takeStep (stepList : List<Point>) (pos: Point) (depthOfTest: int) = 
    printfn "Row: %d Column: %d Risk: %d" pos.Row pos.Column pos.RiskLevel
    if pos.Row = maxRowIndex && pos.Column = maxColumnIndex
    then stepList @ [pos]
    else 
        let arr = generateDownwardValues pos.Row pos.Column [] [] 20 0
        printfn "%A" arr
        let (x,y,_) = arr |> List.head
        let nextPoint = points[x][y]
        takeStep  (stepList @ [nextPoint]) nextPoint depthOfTest
    

let answer1 = 
    takeStep [] (points[0][0]) 1  
    |> List.skip 1
    |> List.sumBy (fun f -> f.RiskLevel)

printfn "Answer 1: %d" answer1

