#time
open System

type PolymerInstruction = {
    Pair: (char * char)
    Insertion : char
}

printfn "Advent of Code Day 14"
let nl = "\n"
let polymerFormulaRaw = 
    System.IO.File.ReadAllText "./input/input_day14.txt" 
    |> (fun s -> s.Split(nl + nl, StringSplitOptions.RemoveEmptyEntries))

let polymerTemplate = polymerFormulaRaw[0]

let pairInsertionRules = 
    polymerFormulaRaw[1].Split(nl, StringSplitOptions.RemoveEmptyEntries)
    |> Array.map(fun instruction -> 
        let instructionPair = instruction.Split(" -> ")
        {
            Pair = (instructionPair[0].ToCharArray()[0], instructionPair[0].ToCharArray()[1])
            Insertion = instructionPair[1].ToCharArray()[0]})

let matchInsertionRule (chars : char array) (index : int) =
    if index = chars.Length - 1 
    then None
    else 
        let insertionRule = 
            pairInsertionRules 
            |> Array.tryFind (fun ir -> (fst ir.Pair) = chars[index] && (snd ir.Pair) = chars[index + 1])
        match insertionRule with 
        | None -> None
        | Some(ir) -> Some(ir.Insertion)

let getLeastCommonAndMostCommon (s: string) =
    let mostCommon = 
        s.ToCharArray()
        |> Array.groupBy id
        |> Array.map (fun (i, is) -> i, (is |> Seq.length))
        |> Array.maxBy snd
    let leastCommon =
        s.ToCharArray()
        |> Array.groupBy id
        |> Array.map (fun (i, is) -> i, (is |> Seq.length))
        |> Array.minBy snd
    (mostCommon, leastCommon)

let printResult (s: string) (step: int) =
    let part = if step = 10 then 1 else 2
    let (mostCommon, leastCommon) = getLeastCommonAndMostCommon s
    printfn "Answer %d: Most common %c occurs %d times and least common %c occurs %d times which results %d" part (fst mostCommon) (snd mostCommon) (fst leastCommon) (snd leastCommon) (snd mostCommon - snd leastCommon)

let rec processInstructions (polymer: string) (step : int) =
    printfn "%s: %d" (DateTime.Now.ToString "HH:mm:ss") step
    let currentChars = polymer.ToCharArray()
    let indexes = [0 .. (currentChars.Length - 1)]
    let newPolymer = 
        indexes
        |> List.fold(fun charArr i ->
            match matchInsertionRule currentChars i with
            | None -> charArr @ [currentChars[i]]
            | Some(ir) -> charArr @ [currentChars[i]; ir]
        ) []
        |> Array.ofList
        |> System.String
    if step = 10
    then 
        printResult newPolymer step
        processInstructions newPolymer (step + 1)
    elif step = 40
    then 
        printResult newPolymer step
    else processInstructions newPolymer (step + 1)

processInstructions polymerTemplate 1