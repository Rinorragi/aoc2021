#time
open System

let dumboOctopusses = 
    System.IO.File.ReadLines "./input/input_day11.txt"
    |> Seq.map (fun f ->  
        f.ToCharArray() 
        |> Array.map (fun f -> 
            f 
            |> sprintf "%c" // Convert to string first for cast to work
            |> int))
    |> Array.ofSeq

let flashEffect (octopus: int) (flashArray: (int * int) array) (previouslyFlashed : (int * int) array) (row : int) (col : int) =
    let flashed = 
        (flashArray |> Array.contains (row, col)) 
        || (previouslyFlashed |> Array.contains (row, col))
    if flashed
    then 0
    else 
        let closeFlash = 
            [   flashArray |> Array.contains ((row - 1), (col - 1)); // top left
                flashArray |> Array.contains ((row - 1), (col)); // top
                flashArray |> Array.contains ((row - 1), (col + 1)); // top right
                flashArray |> Array.contains ((row), (col - 1)); // left
                flashArray |> Array.contains ((row), (col + 1)); // right
                flashArray |> Array.contains ((row + 1), (col - 1)); // bot left
                flashArray |> Array.contains ((row + 1), (col)); // bot
                flashArray |> Array.contains ((row + 1), (col + 1)); // bot right
            ]
        octopus + (closeFlash |> List.filter ((=) true) |> List.length)


let rec flash (dumboOctopusStatus : int array array) (alreadyFlashed : (int * int) array) (flashes : int) =
    // Figure out where flashes happened
    let flashIndexes = 
        dumboOctopusStatus
        |> Array.mapi (fun rowIndex octopusRow -> 
            octopusRow
            |> Array.mapi (fun columnIndex octopus ->
                match octopus > 9 with 
                | true -> Some(rowIndex, columnIndex)
                | _ -> None))
        |> Array.concat
        |> Array.filter (Option.isSome)
        |> Array.map (fun f -> f.Value)
    if flashIndexes.Length > 0
    then 
        // handle flashes
        let newStatus = 
            dumboOctopusStatus
            |> Array.mapi (fun rowIndex octopusRow -> 
                octopusRow
                |> Array.mapi (fun columnIndex octopus -> flashEffect octopus flashIndexes alreadyFlashed rowIndex columnIndex))
        let updatedAlreadyFlashed = Array.concat [flashIndexes; alreadyFlashed]
        flash newStatus updatedAlreadyFlashed (flashes + flashIndexes.Length)
    else 
        (dumboOctopusStatus, flashes) 

let step (dumboOctopusStatus : int array array) =
    // Increase all with one 
    let updatedStatus = 
        dumboOctopusStatus
        |> Array.map (fun octopusRow -> 
            octopusRow |> Array.map ((+) 1))
    flash updatedStatus [||] 0

let rec synchronousFlashFinder (dumboOctopusStatus : int array array) (totalFlashes : int) (stepNumber : int) =
    let stepStatus = step dumboOctopusStatus
    let flashAmount = snd stepStatus
    let newTotalFlashes = totalFlashes + flashAmount
    if(stepNumber = 100) then printfn "Answer 1: %d flashes" newTotalFlashes
    if flashAmount = (dumboOctopusStatus.Length * dumboOctopusStatus[0].Length)
    then stepNumber
    else 
        synchronousFlashFinder (fst stepStatus) newTotalFlashes (stepNumber + 1)


printfn "Advent of Code Day 11"

synchronousFlashFinder dumboOctopusses 0 1 |> printfn "Answer 2: %d step"