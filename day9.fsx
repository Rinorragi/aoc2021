open System

printfn "Advent of Code Day 9"

type Point = {
    Row : int
    Column : int
    Up : Option<int>
    Down : Option<int>
    Left : Option<int>
    Right : Option<int>
    Height : int
    RiskLevel : int
    IsLowPoint : bool
} 

    

let rawPoints = 
    System.IO.File.ReadLines "./input/input_day9.txt"
    |> Seq.map (fun f ->  
        f.ToCharArray() 
        |> Array.map (fun f -> 
            f 
            |> sprintf "%c" // Convert to string first for cast to work
            |> int))
    |> Array.ofSeq

let maxRowIndex = rawPoints.Length - 1
let maxColumnIndex = rawPoints[0].Length - 1

let intToOptionalIntInGrid (grid : int array array) (rowIndex : int) (columnIndex : int) =
    if rowIndex < 0 then None
    elif rowIndex > maxRowIndex then None
    elif columnIndex < 0 then None
    elif columnIndex > maxColumnIndex then None
    else Some(grid[rowIndex][columnIndex])

let unboxOptionalInt (optionalInt : Option<int>) =
    match optionalInt with 
    | Some(x) -> x
    | None -> raise(ArgumentNullException("Unbox only values that you know that are not null"))

let points = 
    rawPoints
    |> Array.mapi (fun rowIndex row -> 
        row |> Array.mapi (fun columnIndex value -> 
            let up = intToOptionalIntInGrid rawPoints (rowIndex - 1) columnIndex
            let down = intToOptionalIntInGrid rawPoints (rowIndex + 1) columnIndex
            let left = intToOptionalIntInGrid rawPoints rowIndex (columnIndex - 1)
            let right = intToOptionalIntInGrid rawPoints rowIndex (columnIndex + 1)
            let value = intToOptionalIntInGrid rawPoints rowIndex columnIndex
            let unboxedValue = value |> unboxOptionalInt
            let minValue = [up;down;left;right;value] |> List.choose id |> List.min
            let amountOfMinValues = [up;down;left;right;value] |> List.choose id |> List.filter ((=) minValue) |> List.length
            {
                Row = rowIndex
                Column = columnIndex
                Up = up
                Down = down
                Left = left
                Right = right
                Height = unboxedValue
                RiskLevel = unboxedValue + 1
                IsLowPoint = amountOfMinValues = 1 && minValue = unboxedValue
            }
        )
    )

let answer1 = 
    points 
    |> Array.concat
    |> Array.filter (fun f -> f.IsLowPoint)
    |> Array.sumBy (fun f -> f.RiskLevel)

printfn "Answer 1: %d" answer1