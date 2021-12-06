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
            seq { 
                for x in minX .. maxX do
                    for y in minY .. maxY do
                        (x,y) }
            |> List.ofSeq
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

let answer = 
    seq {
        for x in 0 .. ventsMaxX do
            for y in 0 .. ventsMaxY do
                let value = Array2D.get onlyHorizontalOrVerticalVentMap x y
                if value > 1 
                then 
                    yield 1
                else 
                    yield 0 }
    |> List.ofSeq
    |> List.sum

printfn "Answer part 1: %d" answer