#time
open System

type CaveSize =
    | Start
    | End
    | Large
    | Small

type Cave = {
    Id: string
    Size: CaveSize
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
    | "start" -> CaveSize.Start
    | "end" -> CaveSize.End
    | _ -> 
        if IsAllUpper caveName 
        then CaveSize.Large
        elif IsAllLower caveName
        then CaveSize.Small
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

let caveNames = 
    cavesRaw
    |> List.map (fun f -> [fst f; snd f])
    |> List.concat
    |> List.distinct

let caves = 
    caveNames
    |> List.map (fun f -> 
        f, {
        Id = f
        Size = solveCaveSize f
        Connections = solveConnections cavesRaw f})
    |> Map.ofList

let routeToString (route: Cave list) = 
    route |> List.map (fun r -> r.Id) |> List.fold (+) "->"

let filterNextCaves (routeResults: Cave list list) (route: Cave list) (nextCave : Cave) = 
    if nextCave.Size = CaveSize.Start
    then false
    elif nextCave.Size = CaveSize.Small && (route |> List.contains nextCave)
    then false
    elif routeResults |> List.map routeToString |> List.exists (fun f -> f.StartsWith(routeToString (route @ [nextCave])))
    then false
    else true

let rec findPaths (routeResults: Cave list list) (route: Cave list) (currentCave : Cave) = 
    let newRoute = route @ [currentCave]
    if currentCave.Size = CaveSize.End
    then  routeResults @ [newRoute]
    else 
        let pathsToContinue =
            currentCave.Connections
            |> List.filter (fun strNextCave -> filterNextCaves routeResults newRoute caves[strNextCave])

        pathsToContinue 
        |> List.fold (fun newResults strNextCave -> 
            if filterNextCaves routeResults newRoute caves[strNextCave]
            then  
                let result = findPaths routeResults newRoute caves[strNextCave]
                result @ newResults
            else 
                newResults
        ) routeResults
        |> List.distinct
            

printfn "Advent of Code Day 12"

let blaa = findPaths [] [] caves["start"]

printfn "Answer 1: %d" blaa.Length