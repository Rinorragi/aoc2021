#time
open System

type CaveType =
    | Start
    | End
    | Large
    | Small

type Cave = {
    Id: string
    Size: CaveType
    Connections : string list
}

let IsAllUpper (caveName : string) = 
    caveName.ToCharArray()
    |> Array.forall(fun f -> Char.IsUpper(f))

let IsAllLower (caveName : string) = 
    caveName.ToCharArray()
    |> Array.forall(fun f -> Char.IsLower(f))

let solveCaveSize (caveName : string) =
    match caveName with
    | "start" -> Start
    | "end" -> End
    | _ -> 
        if IsAllUpper caveName 
        then Large
        elif IsAllLower caveName
        then Small
        else raise(ArgumentException((sprintf "%s is mixed upper and lower case, huh what?" caveName)))
        
let solveConnections (caveConnections: (string * string) list) (caveName: string) =
    let sndConnections = caveConnections |> List.filter (fun f -> fst f = caveName) |> List.map (snd)
    let fstConnections = caveConnections |> List.filter (fun f -> snd f = caveName) |> List.map (fst)
    [fstConnections; sndConnections]
    |> List.concat

let cavesRaw = 
    System.IO.File.ReadLines "./input/input_day12.txt"
    |> List.ofSeq
    |> List.map (fun f ->  
        let caves = f.Split("-", StringSplitOptions.RemoveEmptyEntries)
        (caves[0], caves[1]))

let caves = 
    cavesRaw
    |> List.map (fun f -> [fst f; snd f])
    |> List.concat
    |> List.distinct
    |> List.map (fun f -> 
        f, {
        Id = f
        Size = solveCaveSize f
        Connections = solveConnections cavesRaw f})
    |> Map.ofList

let routeToString (route: Cave list) = 
    route |> List.map (fun r -> r.Id) |> List.fold (+) "->"

let filterNextCaves smallCaveFilter (routeResults: Cave list list) (route: Cave list) (nextCave : Cave) = 
    if nextCave.Size = Start 
        || smallCaveFilter nextCave route
        || routeResults |> List.map routeToString |> List.exists (fun f -> f.StartsWith(routeToString (route @ [nextCave])))
    then false
    else true

let rec findPaths smallCaveFilter (routeResults: Cave list list) (route: Cave list) (currentCave : Cave)  = 
    let newRoute = route @ [currentCave]
    if currentCave.Size = End
    then  routeResults @ [newRoute]
    else 
        let pathsToContinue =
            currentCave.Connections
            |> List.filter (fun strNextCave -> filterNextCaves smallCaveFilter routeResults newRoute caves[strNextCave])

        pathsToContinue 
        |> List.fold (fun newResults strNextCave -> 
            if filterNextCaves smallCaveFilter routeResults newRoute caves[strNextCave]
            then  
                let result = findPaths smallCaveFilter routeResults newRoute caves[strNextCave]
                result @ newResults
            else 
                newResults
        ) routeResults
        |> List.distinct
            

let partOneFilter nextCave route = nextCave.Size = Small && (route |> List.contains nextCave)
let partTwoFilter nextCave route = 
    nextCave.Size = Small
    && (route |> List.contains nextCave)
    && (route 
        |> List.filter (fun f -> f.Size = Small) 
        |> List.groupBy (fun f -> f.Id) 
        |> List.exists (fun f -> 
            (snd f) |> List.length > 1))

printfn "Advent of Code Day 12"

let answer1 = findPaths partOneFilter [] [] caves["start"] 
printfn "Answer 1: %d" answer1.Length
let answer2 = findPaths partTwoFilter [] [] caves["start"] 
printfn "Answer 2: %d" answer2.Length