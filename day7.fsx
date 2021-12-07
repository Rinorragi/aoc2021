open System

printfn "Advent of Code Day 7"

let crabMarinesInput = 
    System.IO.File.ReadAllText "./input/input_day7.txt" 
    |> fun (s:string) -> s.Split(",", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int
    |> List.ofArray

let part1CalculationFunc (crabMarine: int) (pos: int) =
    crabMarine - pos |> abs

let part2CalculationFunc (crabMarine: int) (pos: int) =
    let distance = part1CalculationFunc crabMarine pos
    [0 .. distance] |> List.sum

let calculateFuelUsageByPosition calculationFunc (crabMarines : int list)  = 
    let minPos = crabMarines |> List.min
    let maxPos = crabMarines |> List.max
    [minPos .. maxPos]
    |> List.map (fun pos -> 
        crabMarines 
        |> List.map (fun crabMarine -> calculationFunc crabMarine pos)
        |> List.sum)

let answer = crabMarinesInput |> calculateFuelUsageByPosition part1CalculationFunc |> List.min
printfn "Answer 1: %d" answer

let answer2 = crabMarinesInput |> calculateFuelUsageByPosition part2CalculationFunc |> List.min
printfn "Answer 2: %d" answer2