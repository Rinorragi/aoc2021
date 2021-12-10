#time
open System

type BracketType =
    | Round
    | Square
    | Curly
    | Angle

type IndexedChar = {
    Type : BracketType
    OriginalChar : char
    Index : int
    IsOpening : bool
    IsRemoved : bool
}

type SyntaxRow = {
    Chars : IndexedChar list
    OriginalRow : string
}

let charToBracketType (c : char) = 
    match c with 
    | '(' | ')' -> BracketType.Round
    | '[' | ']' -> BracketType.Square
    | '{' | '}' -> BracketType.Curly
    | '<' | '>' -> BracketType.Angle
    | _ -> raise(ArgumentException((sprintf "Invalid character type %c" c)))

let charIsOpening (c : char) =
    match c with 
    | '(' | '[' | '{' | '<' -> true
    | ')' | ']' | '}' | '>' -> false
    | _ -> raise(ArgumentException((sprintf "Invalid character type %c" c)))

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

let navSyntaxRows = 
    System.IO.File.ReadLines "./input/input_day10.txt"
    |> List.ofSeq
    |> List.map (fun s ->
        {
            Chars = 
                s.ToCharArray()
                |> Array.mapi (fun i c -> {
                    Type = charToBracketType c
                    OriginalChar = c
                    Index = i
                    IsRemoved = false
                    IsOpening = charIsOpening c})
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

navSyntaxRows
|> List.map (fun sr -> tidyUp sr.Chars)
|> List.map (fun ics -> 
    let nonOpenings = 
        ics 
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