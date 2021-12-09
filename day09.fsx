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
    Height : int
    RiskLevel : int
    IsLowPoint : bool
} 

let rawPoints = 
    System.IO.File.ReadLines "./input/input_day09.txt"
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

let points = 
    rawPoints
    |> Array.mapi (fun rowIndex row -> 
        row |> Array.mapi (fun columnIndex value -> 
            let up = positionToOptionalInt rawPoints (rowIndex - 1) columnIndex
            let down = positionToOptionalInt rawPoints (rowIndex + 1) columnIndex
            let left = positionToOptionalInt rawPoints rowIndex (columnIndex - 1)
            let right = positionToOptionalInt rawPoints rowIndex (columnIndex + 1)
            let value = positionToOptionalInt rawPoints rowIndex columnIndex
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
                Id = sprintf "%d,%d" rowIndex columnIndex
            }
        )
    )

let isIntSmallerThanOptionalInt (value: int) (optionalValue: Option<int>)  =
    match optionalValue with
    | Some(x) ->  x < 9 && value < x
    | None -> false

let joinMaps (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    (Seq.concat [(Map.toSeq p); (Map.toSeq q)]) |> Map.ofSeq

let rec solveBasin (basinPoints : Map<string,Point>) (p : Point) =
    if basinPoints.ContainsKey(p.Id)
    then 
        basinPoints
    else 
        let updatedBasinPoints = basinPoints |> Map.add p.Id p
        let up = if isIntSmallerThanOptionalInt p.Height p.Up then Some(((p.Row - 1), p.Column)) else None
        let down = if isIntSmallerThanOptionalInt p.Height p.Down then Some(((p.Row + 1), p.Column)) else None
        let left = if isIntSmallerThanOptionalInt p.Height p.Left then Some(p.Row, (p.Column - 1)) else None
        let right = if isIntSmallerThanOptionalInt p.Height p.Right then Some(p.Row, (p.Column + 1)) else None
        [up;down;left;right]
            |> List.filter (Option.isSome)
            |> List.map (fun f -> points[fst f.Value][snd f.Value])
            |> List.filter (fun p -> not(basinPoints.ContainsKey(p.Id)))
            |> List.fold (fun basinPointUpdater newPoint -> 
                joinMaps basinPointUpdater (solveBasin basinPointUpdater newPoint)
            ) updatedBasinPoints

printfn "Advent of Code Day 9"

let answer1 = 
    points 
    |> Array.concat
    |> Array.filter (fun f -> f.IsLowPoint)
    |> Array.sumBy (fun f -> f.RiskLevel)

printfn "Answer 1: %d" answer1

let answer2 = 
    points 
    |> Array.concat
    |> Array.filter (fun f -> f.IsLowPoint)
    |> Array.map (fun f -> solveBasin (Map []) f)
    |> Array.map (fun f -> f |> Map.toSeq |> Seq.length)
    |> Array.sortDescending
    |> Array.take 3
    |> Array.reduce (*)

printfn "Answer 2: %d" answer2