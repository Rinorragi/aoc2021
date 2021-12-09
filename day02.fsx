#time
open System

exception RecursionProblem of string

let filterList (cmdList : string list) (keyword : string) =
    cmdList
        |> List.filter(fun x -> x.StartsWith(keyword))
        |> List.map (fun x -> x.Split(" ")[1])
        |> List.map System.Int32.Parse
        |> List.sum 

let rec submarineNavigation (cmdList : string list) (aim : int) (horizontal : int) (depth : int) =
    if cmdList.Length = 0 || String.IsNullOrWhiteSpace cmdList.Head
    then
        (horizontal, depth)
    else
        let cmdLine = cmdList.Head.Split(" ")
        let cmd = cmdLine[0]
        let cmdValue = cmdLine[1] |> System.Int32.Parse
        // printfn "Command '%s' with value %d aim %d horizontal %d depth %d" cmd cmdValue aim horizontal depth 
        match cmd with
        | "forward" -> 
            submarineNavigation cmdList.Tail aim (horizontal + cmdValue) (depth + (aim * cmdValue))
        | "up" ->
            submarineNavigation cmdList.Tail (aim - cmdValue) horizontal depth
        | "down" ->
            submarineNavigation cmdList.Tail (aim + cmdValue) horizontal depth
        | _ -> raise (RecursionProblem(sprintf "Should not be possible, command was: '%s' with value: '%d'" cmd cmdValue))
        
printfn "Advent of Code Day 2"
let commandList = 
    System.IO.File.ReadLines "./input/input_day02.txt"
    |> Seq.toList

let horizonal = filterList commandList "forward"
let verticalDowns = filterList commandList "down"
let verticalUps = filterList commandList "up"
let answer1 = horizonal * (verticalDowns - verticalUps)
printfn "Answer 1: %d" answer1

let navigationResult = submarineNavigation commandList 0 0 0
let answer2 = fst navigationResult * snd navigationResult
printfn "Answer 2: %d" answer2

