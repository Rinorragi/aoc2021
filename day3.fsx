open System

exception ValueProblem of string

let stringToIntList (reading: string) =
    reading
    |> Seq.toList
    |> List.map (fun x -> 
        match x with
        | '0' -> 0
        | '1' -> 1
        | _ -> raise(ValueProblem(sprintf "Value %c is not possible" x))
    )

let tupleToBit (state: (int * int)) =
    if fst state > snd state 
        then 
            0
        else
            1

let bitArrayToInt (aBitList: int list) =
    aBitList
    |> List.rev
    |> List.fold (fun acc elem ->
        // index, sum
        let index = fst acc
        (index + 1, snd acc + elem * (pown 2 index))) 
        (0, 0)
    |> snd 

let calculateMostCommonBitList (aBitList: (int list) list) = 
    let listLength = aBitList.Head.Length
    aBitList
    |> List.map (fun x -> 
        x 
        |> List.map (fun y -> 
            match y with
            | 0 -> (1, 0)
            | 1 -> (0, 1)
            | _ -> raise(ValueProblem(sprintf "Value %d is not possible" y))))
    |> List.fold (fun acc elem ->     
        elem 
        |> List.mapi (fun i x -> fst acc[i] + fst x, snd acc[i] + snd x)) 
        (List.ofArray (Array.create (listLength + 1) (0, 0)))
    |> List.map tupleToBit

let reverseBitList (bitList: int list) =
    bitList 
    |> List.map (fun x -> 
    match x with 
    | 0 -> 1
    | 1 -> 0
    | _  -> raise(ValueProblem(sprintf "Value %d is not possible" x)))

printfn "Advent of Code Day 3"
let commandList = 
    System.IO.File.ReadLines "./input/input_day3.txt"
    |> Seq.toList
    |> List.map stringToIntList

// gamma rate = most common bit "in the corresponding position"
let gammaRateBitList =
    commandList
    |> (calculateMostCommonBitList)

// epsilon rate = least common bit "in the corresponding position"
let epsilonRateBiList = 
    gammaRateBitList
    |> reverseBitList

let gammaRate = bitArrayToInt gammaRateBitList
let epsilonRate = bitArrayToInt epsilonRateBiList

// power consumption = gamma rate * epsilon rate
let powerConsumption = gammaRate * epsilonRate

printfn "Answer 1: %d"  powerConsumption

let valueLength = commandList.Head.Length
// Oxygen rating filter by most common value until only 1 left
let oxygenRatingList =
    [0 .. 1 .. valueLength - 1]
    |> List.fold (fun (acc: (int list) list) (elem: int) -> 
        if acc.Length = 1
        then 
            acc
        else 
            let mostCommonBitList = calculateMostCommonBitList acc
            acc 
            |> List.filter (fun f -> f[elem] = mostCommonBitList[elem])
    ) commandList
// CO2 Scrubber value filter by least common value until 1 left
let co2ScrubberList =
    [0 .. 1 .. valueLength - 1]
    |> List.fold (fun (acc: (int list) list) (elem: int) -> 
        if acc.Length = 1
        then 
            acc
        else 
            let leastCommonBitList = 
                acc 
                |> calculateMostCommonBitList
                |> reverseBitList
            acc 
            |> List.filter (fun f -> f[elem] = leastCommonBitList[elem])
    ) commandList
// Life support rating = oxygen rating * co2 scrubber
let oxygenRating = bitArrayToInt oxygenRatingList.Head
let co2ScrubberValue = bitArrayToInt co2ScrubberList.Head

let lifeSupportRating = oxygenRating * co2ScrubberValue

printfn "Answer 2: %d" lifeSupportRating 

