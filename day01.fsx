#time
open System

let compareSum (refList : int list, indx : int) =
    refList.Item(indx-2) + refList.Item(indx-1) + refList.Item(indx) > refList.Item(indx-3) + refList.Item(indx-2) + refList.Item(indx-1)

printfn "Advent of Code Day 1"
let depthList : int list = 
    System.IO.File.ReadLines "./input/input_day01.txt"
    |> Seq.map System.Int32.Parse
    |> List.ofSeq

let increases = 
    depthList
    |> Seq.pairwise
    |> Seq.filter (fun aDepth -> snd (aDepth) > fst (aDepth)) 

let answer1 = increases |> Seq.length
printfn "Answer 1: %d" answer1

let increasesPart2 = 
    depthList
    |> List.mapi (fun i x -> if i > 2 then Some(compareSum (depthList, i)) else None)
    |> List.filter Option.isSome
    |> List.map Option.get
    |> List.filter (fun x -> x = true)

let answer2 = increasesPart2 |> Seq.length
printfn "Answer 2: %d" answer2