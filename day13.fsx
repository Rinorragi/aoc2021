#time
open System


printfn "Advent of Code Day 13"
let heatCameraManualRaw = 
    System.IO.File.ReadAllText "./input/input_day13.txt"

let nl = "\n"
let heatCameraManual = 
    heatCameraManualRaw.Split(nl + nl, StringSplitOptions.RemoveEmptyEntries)

let dots = 
    heatCameraManual[0].Split(nl)
    |> Array.map (fun row ->
        let rowSplit = row.Split(",", StringSplitOptions.RemoveEmptyEntries)
        (rowSplit[0] |> int, rowSplit[1] |> int))
    |> List.ofArray

let folds =
    heatCameraManual[1].Split(nl, StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun row -> 
        (row.ToCharArray()[11], row.Split("=")[1] |> int))
    |> List.ofArray

let foldHorizontally (dotList: (int * int) list) (foldY: int) =
    dotList 
    |> List.map (fun (x,y) ->
        let newXY =
            match y > foldY with
            | true -> (x, ((2*foldY) - y))
            | false -> (x,y)
        newXY)

let foldVertically (dotList: (int * int) list) (foldX: int) =
    dotList 
    |> List.map (fun (x,y) ->
        let newXY =
            match x > foldX with
            | true -> (((2*foldX) - x), y)
            | false -> (x,y)
        newXY)

let foldDots (dotList : (int * int) list) (fold: char * int) =
    match fst fold with
    | 'x' -> foldVertically dotList (snd fold)
    | 'y' -> foldHorizontally dotList (snd fold)
    | _ -> raise(ArgumentException((sprintf "%c is illegal in fold") (fst fold)))
    |> List.distinct

let foldedManualPage = 
    folds 
    |> List.fold (fun foldedDots fold -> 
        let newDots = foldDots foldedDots fold
        printfn "Fold %c at %d has visible dots %d" (fst fold) (snd fold) newDots.Length
        newDots
    ) dots

let manualPage =
    foldedManualPage
    |> List.map (fun (x,y) -> 
        ((x,y),true))
    |> Map.ofList

let maxY = foldedManualPage |> List.maxBy snd |> snd
let maxX = (foldedManualPage |> List.maxBy fst |> fst)
[0 .. maxY]
|> List.map (fun y -> 
    [0 .. maxX]
    |> List.map (fun x -> 
        match manualPage.ContainsKey(x,y) with
        | true -> printf "#"
        | false -> printf " ")
    |> ignore
    printfn "")