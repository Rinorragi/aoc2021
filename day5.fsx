open System

exception DataExpectationException of string

type VentLine = {
    StartX: int
    EndX : int
    StartY: int
    EndY : int
    MinX : int
    MaxX : int
    MinY : int
    MaxY : int
    IsHorizontal : bool
    IsVertical : bool
    Coordinates : (int * int) list
} with 
    static member InitVent (ventRow: string) = 
        let ventArray = 
            ventRow.Split(" -> ")
            |> Array.map (fun f -> 
                f.Split(",")
                |> Array.map int)
        let isHorizontal = ventArray[0][0] = ventArray[1][0]
        let isVertical = ventArray[0][1] = ventArray[1][1]
        let minX = [ventArray[0][0]; ventArray[1][0]] |> List.min
        let maxX = [ventArray[0][0]; ventArray[1][0]] |> List.max
        let minY = [ventArray[0][1]; ventArray[1][1]] |> List.min
        let maxY = [ventArray[0][1]; ventArray[1][1]] |> List.max
        let coordinatesList : (int * int) list  = 
            [ for x in minX .. maxX do
                for y in minY .. maxY do
                    (x,y) ]
        {
            StartX = ventArray[0][0]
            StartY = ventArray[0][1]
            EndX = ventArray[1][0]
            EndY = ventArray[1][1]
            MinX = minX
            MaxX = maxX
            MinY = minY
            MaxY = maxY
            IsHorizontal = isHorizontal
            IsVertical = isVertical
            Coordinates = coordinatesList
        }

let answerCalculation (ventMap : int[,]) (maxX : int) (maxY : int) = 
    [ for x in 0 .. maxX do
        for y in 0 .. maxY do
            if (Array2D.get ventMap x y) > 1 
            then
                yield 1 ]
    |> List.sum

printfn "Advent of Code Day 5"

let ventLines = 
    System.IO.File.ReadLines "./input/input_day5.txt"
    |> Seq.toList
    |> List.map VentLine.InitVent

let ventsMaxX = (ventLines |> List.maxBy (fun f -> f.MaxX)).MaxX
let ventsMaxY = (ventLines |> List.maxBy (fun f -> f.MaxY)).MaxY

let emptyVentMap = Array2D.create (ventsMaxX+1) (ventsMaxY+1) 0
let onlyHorizontalOrVerticalVentMap = 
    ventLines
    |> List.filter (fun f -> f.IsHorizontal || f.IsVertical)
    |> List.fold (fun ventMap vent -> 
        for xy in vent.Coordinates do
            let oldValue = (Array2D.get ventMap (fst xy) (snd xy))
            Array2D.set ventMap (fst xy) (snd xy) (oldValue + 1)
        ventMap
    ) emptyVentMap

let answer = answerCalculation onlyHorizontalOrVerticalVentMap ventsMaxX ventsMaxY

printfn "Answer part 1: %d" answer

let diagonalsAddedVentMap =
    ventLines
    |> List.filter (fun f -> not(f.IsHorizontal) && not(f.IsVertical))
    |> List.fold (fun ventMap vent ->
        let xInc = if vent.StartX < vent.EndX then 1 else -1
        let yInc = if vent.StartY < vent.EndY then 1 else -1
        let rounds = abs (vent.StartX - vent.EndX)
        let updatedMap = 
            [0 .. rounds]
            |> List.fold (fun acc _ -> 
                let currentMap, x, y = acc
                let oldValue = (Array2D.get ventMap x y)
                Array2D.set currentMap x y (oldValue + 1)
                (currentMap, x + xInc, y + yInc)
            ) (ventMap, vent.StartX, vent.StartY)
        match updatedMap with
        | (newMap, _, _ ) -> newMap
    ) onlyHorizontalOrVerticalVentMap

let answer2 = answerCalculation diagonalsAddedVentMap ventsMaxX ventsMaxY

printfn "Answer part 2: %d" answer2
