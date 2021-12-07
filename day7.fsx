open System

printfn "Advent of Code Day 7"

let crabMarines = 
    System.IO.File.ReadAllText "./input/input_day7.txt" 
    |> fun (s:string) -> s.Split(",", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int
    |> List.ofArray

let minPos = crabMarines |> List.min
let maxPos = crabMarines |> List.max

let part1CalculationFunc (crabMarine: int) (pos: int) =
    crabMarine - pos |> abs

let part2CalculationFunc (crabMarine: int) (pos: int) =
    let distance = part1CalculationFunc crabMarine pos
    match distance with
    | 0 -> 0
    | x -> [0 .. x] |> List.sum

let calculateFuelUsageByPosition calculationFunc = 
    [minPos .. maxPos]
    |> List.map (fun pos -> 
        let fuelSum = 
            crabMarines 
            |> List.map (fun crabMarine -> calculationFunc crabMarine pos)
            |> List.sum
        (pos,fuelSum))

let answer = calculateFuelUsageByPosition part1CalculationFunc |> List.minBy snd |> snd
printfn "Answer 1: %d" answer

let answer2 = calculateFuelUsageByPosition part2CalculationFunc |> List.minBy snd |> snd
printfn "Answer 2: %d" answer2