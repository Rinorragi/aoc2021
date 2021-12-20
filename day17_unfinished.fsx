#time
open System


let transmission = 
    System.IO.File.ReadLines "./input/input_day16.txt"
    |> List.ofSeq
    |> List.head
    |> (fun s -> s.ToCharArray())
    |> Array.map (fun c ->
        match c with 
        | '0' -> "0000"
        | '1' -> "0001"
        | '2' -> "0010"
        | '3' -> "0011"
        | '4' -> "0100"
        | '5' -> "0101"
        | '6' -> "0110"
        | '7' -> "0111"
        | '8' -> "1000"
        | '9' -> "1001"
        | 'A' -> "1010"
        | 'B' -> "1011"
        | 'C' -> "1100"
        | 'D' -> "1101"
        | 'E' -> "1110"
        | 'F' -> "1111"
        | _ -> raise(ArgumentException(sprintf "%c is invalid" c)))

// 3 bits for packet version
// 3 bits for packet type ID
// type id 4 is literal, ultiple of four bits
// other types are operators

printfn "%A" transmission