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

let polymerTemplate = 
    polymerFormulaRaw[0].ToCharArray() 
    |> Array.pairwise 
    |> Array.map (fun f -> [|fst f; snd f|] |> System.String)
    |> Array.groupBy id
    |> Array.map (fun f -> fst f, snd f |> Array.length |> int64)
    |> Map.ofArray

let pairInsertionRules = 
    polymerFormulaRaw[1].Split(nl, StringSplitOptions.RemoveEmptyEntries)
    |> Array.map(fun instruction -> 
        let instructionPair = instruction.Split(" -> ")
        instructionPair[0], 
        {
            Pair = (instructionPair[0].ToCharArray()[0], instructionPair[0].ToCharArray()[1])
            Insertion = instructionPair[1].ToCharArray()[0]})
    |> Map.ofArray

let upsertToMap (aMap: Map<'a,int64>) (key: 'a) (aValue: int64) =
    match aMap.ContainsKey(key) with
    | true ->  aMap |> Map.add key (aMap[key] + aValue)
    | false -> aMap |> Map.add key aValue

let getLeastCommonAndMostCommon (aMap: Map<string,int64>) =
    let charList = 
        aMap 
        |> Map.fold (fun (state: Map<char,int64>) key value -> 
            let elem1State = upsertToMap state (key.ToCharArray()[0]) value
            upsertToMap elem1State (key.ToCharArray()[1]) value
        ) ([] |> Map.ofList)
        |> Map.toList
    let mostCommon = 
        charList
        |> List.maxBy snd
    let leastCommon =
        charList
        |> List.minBy snd
    (mostCommon, leastCommon)

let printResult (aMap: Map<string,int64>) (step: int) =
    let part = if step = 10 then 1 else 2
    let (mostCommon, leastCommon) = getLeastCommonAndMostCommon aMap
    // Characters appears always in pairs, so we have double too much of them
    let least = floor ((double)(snd leastCommon) / (double)2)
    let most = ceil ((double)(snd mostCommon) / (double)2)
    printfn "Answer %d: Most common %c occurs %f times and least common %c occurs %f times which results %f" part (fst mostCommon) most (fst leastCommon) least (most - least)

let rec processInstructions (polymer: Map<string,int64>) (step: int) =
    let newPolymer = 
        polymer
        |> Map.fold (fun state key value -> 
            let ir = pairInsertionRules[key]
            let elem1 = [|fst ir.Pair; ir.Insertion|] |> System.String
            let elem2 = [|ir.Insertion; snd ir.Pair|] |> System.String
            let elem1State = upsertToMap state elem1 value 
            upsertToMap elem1State elem2 value
        ) ([] |> Map.ofList)
    if step = 10
    then 
        printResult newPolymer step
        processInstructions newPolymer (step + 1)
    elif step = 40
    then 
        printResult newPolymer step
    else processInstructions newPolymer (step + 1)

processInstructions polymerTemplate 1