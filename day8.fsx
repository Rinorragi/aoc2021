open System
open System.Collections.Generic

printfn "Advent of Code Day 8"

type SignalEntry = {
    SignalPatterns : string array
    OutputValues: string array
    SolvedValues: IDictionary<int, string>
}

let intersectBy (patternToMatch: string) (pattern: string) (count: int) =
    (Set.intersect (set (patternToMatch.ToCharArray())) (set (pattern.ToCharArray()))).Count = count

let findMissingChar (patternToMatch: string) (removeThisPattern: string) = 
    Set.difference (set (patternToMatch.ToCharArray())) (set (removeThisPattern.ToCharArray())) |> Set.toArray |> Array.head

let solveSignalPatterns (signalPatterns : string array) =
    let onePattern = signalPatterns |> Array.find (fun f -> f.Length = 2)
    let fourPattern = signalPatterns |> Array.find (fun f -> f.Length = 4)
    let sevenPattern = signalPatterns |> Array.find (fun f -> f.Length = 3)
    let eightPattern = signalPatterns |> Array.find (fun f -> f.Length = 7)
    // 0, 6 and 9
    let sixtLengthPatterns = signalPatterns |> Array.filter (fun f -> f.Length = 6)
    // 2, 3 and 5
    let fiveLengthPatterns = signalPatterns |> Array.filter (fun f -> f.Length = 5)
    // Six is the one that is missing upper right char which we can find by comparing to 1
    let sixPattern = sixtLengthPatterns |> Array.find (fun f -> intersectBy onePattern f 1)
    // Comparing eight and 6 we get the upper right char
    let upperRightChar = findMissingChar eightPattern sixPattern
    // Removing the known six pattern we can deduce the 0 pattern by comparing 9 to four
    let ninePattern = sixtLengthPatterns |> Array.filter (fun f -> f <> sixPattern) |> Array.find (fun f -> intersectBy f fourPattern 4)
    // Only zero remaining
    let zeroPattern = sixtLengthPatterns |> Array.find (fun f -> f <> sixPattern && f <> ninePattern)
    // From five length list, 3 is only one to match entirely to 1
    let threePattern = fiveLengthPatterns |> Array.find (fun f -> intersectBy f onePattern 2)
    // Upper right char is missing from 5 but exists in 2
    let twoPattern = fiveLengthPatterns |> Array.filter (fun f -> f <> threePattern) |> Array.find (fun f -> intersectBy f (upperRightChar.ToString()) 1)
    // Only 5 remaining
    let fivePattern = fiveLengthPatterns |> Array.find (fun f -> f <> threePattern && f <> twoPattern)

    [   0, zeroPattern
        1, onePattern; 
        2, twoPattern;
        3, threePattern
        4, fourPattern;
        5, fivePattern; 
        6, sixPattern;
        7, sevenPattern; 
        8, eightPattern
        9, ninePattern] |> dict


let stringCharactersMatch (pattern: string) (value: string) =
    let interSection = Set.intersect (set (pattern.ToCharArray())) (set (value.ToCharArray()))
    [pattern.Length; value.Length; interSection.Count]
    |> List.forall ((=) pattern.Length)

let countUsageOfNumbers (entry : SignalEntry) (numbers : int list) =
    numbers 
    |> List.map (fun number -> 
        let pattern = entry.SolvedValues[number]
        entry.OutputValues |> Array.filter (fun f -> stringCharactersMatch pattern f) |> Array.length)
    |> List.sum

let patternValueToLong (entry : SignalEntry) =
    let invertedDictionary = entry.SolvedValues |> Seq.map (fun (KeyValue(k, v)) -> (v,k)) |> dict
    entry.OutputValues 
    |> Array.map (fun value -> 
        entry.SignalPatterns 
        |> Array.filter (fun pattern -> stringCharactersMatch pattern value )
        |> Array.map (fun f -> invertedDictionary[f])
        |> Array.head)
    |> Array.map string
    |> String.concat ""
    |> int64

let signalEntries = 
    System.IO.File.ReadLines "./input/input_day8.txt"
    |> Seq.map (fun f -> 
        let row = f.Split('|',StringSplitOptions.RemoveEmptyEntries)
        let patterns = row[0].Split(" ", StringSplitOptions.RemoveEmptyEntries) 
        let values = row[1].Split(" ", StringSplitOptions.RemoveEmptyEntries)
        {
            SignalPatterns = patterns
            OutputValues = values
            SolvedValues = solveSignalPatterns patterns })

let answer1 = 
    signalEntries
    |> Seq.map (fun f -> countUsageOfNumbers f [1;4;7;8] )
    |> Seq.sum

printfn "Answer 1: %d" answer1

let answer2 =
    signalEntries 
    |> Seq.map patternValueToLong
    |> Seq.sum

printfn "Answer 2: %d" answer2