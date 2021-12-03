open System

exception ValueProblem of string

let commonBitCalculation (reading: string) =
    reading
    |> Seq.toList
    |> List.mapi (fun i x -> 
        match x with
        | '0' -> (1, 0)
        | '1' -> (0, 1)
        | _ -> raise(ValueProblem(sprintf "Value %c is not possible" x))
    )

let tupleToBit (state: (int * int)) =
    if fst state > snd state 
        then 
            0
        else
            1

let bitArrayToInt (aBitArray: int list) =
    aBitArray
    |> List.rev
    |> List.fold (fun acc elem ->
        // index, sum
        let index = fst acc
        (index + 1, snd acc + elem * (pown 2 index))) 
        (0, 0)
    |> snd 

printfn "Advent of Code Day 3"
let commandList = 
    System.IO.File.ReadLines "./input/input_day3.txt"
    |> Seq.toList

let valueLength = commandList.Head.Length
let standardSizedArray = Array.create (valueLength + 1) (0, 0)

// gamma rate = most common bit "in the corresponding position"
let gammaRateBitArray =
    commandList
    |> List.map commonBitCalculation
    |> List.fold (fun acc elem ->     
        elem 
        |> List.mapi (fun i x -> fst acc[i] + fst x, snd acc[i] + snd x)) (List.ofArray standardSizedArray)
    |> List.map tupleToBit

// epsilon rate = least common bit "in the corresponding position"
let epsilonRateBitArray = 
    gammaRateBitArray
    |> List.map (fun x -> 
        match x with 
        | 0 -> 1
        | 1 -> 0
        | _  -> raise(ValueProblem(sprintf "Value %d is not possible" x)))

printfn "Epsilon rate %A" epsilonRateBitArray

let gammaRate = bitArrayToInt gammaRateBitArray
let epsilonRate = bitArrayToInt epsilonRateBitArray

// power consumption = gamma rate * epsilon rate
let powerConsumption = gammaRate * epsilonRate

printfn "Answer 1: %d"  powerConsumption

//printfn "Answer 2: %d" answer2

