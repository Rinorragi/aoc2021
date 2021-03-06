#time
open System

type IndexedChar = {
    OriginalChar : char
    Index : int
    IsOpening : bool
}

type SyntaxRow = {
    Chars : IndexedChar list
    OriginalRow : string
}

let indexedCharToSyntaxErrorScore (ic : IndexedChar) =
    match ic.OriginalChar with 
    | '(' | ')' -> 3
    | '[' | ']' -> 57
    | '{' | '}' -> 1197
    | '<' | '>' -> 25137
    | _ -> raise(ArgumentException((sprintf "Invalid character type %c" ic.OriginalChar)))

let matchIndexedCharTuple (icTuple: IndexedChar * IndexedChar) =
    ((fst icTuple).OriginalChar = '(' && (snd icTuple).OriginalChar = ')')
    || ((fst icTuple).OriginalChar = '[' && (snd icTuple).OriginalChar = ']')
    || ((fst icTuple).OriginalChar = '{' && (snd icTuple).OriginalChar = '}')
    || ((fst icTuple).OriginalChar = '<' && (snd icTuple).OriginalChar = '>')

let charToAutoCompleteScore (c : char) = 
    match c with 
    | '(' -> 1L
    | '[' -> 2L
    | '{' -> 3L
    | '<' -> 4L
    | _ -> raise(ArgumentException((sprintf "Invalid character type %c" c)))

let navSyntaxRows = 
    System.IO.File.ReadLines "./input/input_day10.txt"
    |> List.ofSeq
    |> List.map (fun s ->
        {
            Chars = 
                s.ToCharArray()
                |> Array.mapi (fun i c -> {
                    OriginalChar = c
                    Index = i
                    IsOpening =  
                        match c with 
                        | '(' | '[' | '{' | '<' -> true
                        | ')' | ']' | '}' | '>' -> false
                        | _ -> raise(ArgumentException((sprintf "Invalid character type %c" c)))})
                |> List.ofArray
            OriginalRow = s})

let rec tidyUp (currentRow : IndexedChar list) =
    let toRemove =
        currentRow 
        |> List.pairwise
        |> List.filter matchIndexedCharTuple
        |> List.map (fun f -> [(fst f);(snd f)])
        |> List.concat
    if toRemove.Length = 0
    then currentRow
    else 
        currentRow
        |> List.except toRemove
        |> tidyUp

let unboxOptionalIndexedChar (oc : Option<IndexedChar>) =
    match oc with 
    | Some(x) -> x
    | None -> raise(ArgumentNullException("Unbox only values that you know that are not null"))

printfn "Advent of Code Day 10"

let indexedSyntaxRows = 
    navSyntaxRows 
    |> List.map (fun sr -> (sr, tidyUp sr.Chars))

indexedSyntaxRows
|> List.map (fun ics -> 
    let nonOpenings = 
        (snd ics) 
        |> List.filter (fun ic -> not(ic.IsOpening))
    if nonOpenings.Length = 0
    then None
    else Some(nonOpenings.Head)
)
|> List.filter Option.isSome
|> List.map unboxOptionalIndexedChar
|> List.map indexedCharToSyntaxErrorScore
|> List.sum
|> printfn "Answer 1: %d"

let incompleteRows = 
    indexedSyntaxRows
    |> List.filter (fun f -> (snd f) |> List.forall(fun c -> c.IsOpening))

let answerList =
    incompleteRows
    |> List.map (fun f -> 
        let charsToComplete = (snd f)
        charsToComplete
        |> List.rev
        |> List.fold(fun score elem -> 
            (5L * score) + (charToAutoCompleteScore elem.OriginalChar)  
        ) 0L
    )
    |> List.sort

let answer2 = answerList[answerList.Length / 2]
printfn "Answer 2: %d" answer2