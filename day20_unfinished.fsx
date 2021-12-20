#time
open System

printfn "Advent of Code Day 20"
let nl = "\n"
let imageScannerRawData = 
    System.IO.File.ReadAllText "./input/input_day20.txt" 
    |> (fun s -> s.Split(nl + nl, StringSplitOptions.RemoveEmptyEntries))

let imageEnhancementAlgorithm = imageScannerRawData[0]
let inputImage = 
    imageScannerRawData[1] 
    |> (fun s -> s.Split(nl, StringSplitOptions.RemoveEmptyEntries))

printfn "%s" imageEnhancementAlgorithm
printfn "%A" inputImage