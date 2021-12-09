#time
open System

let checkBingoBoardForWin (bingoBoard: (int * bool) list list) = 
    // Check for winning row
    let winningRow = 
        bingoBoard
        |> List.filter (fun row -> 
            row |> List.forall(fun elem -> snd elem))
        |> List.length > 0
    let twoDimensionalArray = Array2D.init 5 5 (fun i j -> bingoBoard[i][j])
    let winningColumn = 
        [0 .. 4]
        |> List.filter (fun column -> 
            twoDimensionalArray[*,column]
            |> Array.forall(fun elem -> snd elem))
        |> List.length > 0
    // Return if this board won
    winningRow || winningColumn

let updateBoardsWithNumber (bingoBoards: (int * bool) list list list) (number: int) = 
    bingoBoards
    |> List.map (fun bingoBoard ->
        bingoBoard 
        |> List.map (fun bingoRow -> 
            bingoRow
            |> List.map (fun bingoBoardNumber -> 
                if fst bingoBoardNumber = number
                then 
                    (number, true)
                else 
                    bingoBoardNumber
            )
        )
    )

let getUnusedNumbers (bingoBoard: (int * bool) list list) = 
    bingoBoard
    |> List.map (fun row -> 
        row
        |> List.filter (fun number -> snd number = false))
    |> List.concat
    |> List.map fst

printfn "Advent of Code Day 4"
let bingoRawTextInput = 
    System.IO.File.ReadAllText "./input/input_day04.txt"

let nl = "\n"
let bingoRawInput = 
    bingoRawTextInput.Split(nl + nl)

let bingoNumbers = 
    bingoRawInput[0].Split(",")
    |> Array.toList
    |> List.map int

let bingoBoards = 
    bingoRawInput
    |> Array.skip 1 // the bingo number input
    |> Array.map (fun x -> 
        x.Split(nl, StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun y -> 
            [|y[0..1]; y[3..4];y[6..7];y[9..10];y[12..13]|]
            |> Array.map (fun f -> f.Trim())
            |> Array.map int
            |> Array.map (fun f -> (f, false))
            |> List.ofArray)
        |> List.ofArray)
    |> List.ofArray

printfn "Created bingo boards"

let winner = 
    bingoNumbers
    |> List.fold(fun bingoBoardsAndNumber number ->
        let listOfBingoBoards = fst bingoBoardsAndNumber
        // If we already have a winner then return it unupdated
        if List.length listOfBingoBoards = 1
        then 
            bingoBoardsAndNumber
        else 
            // Update boards
            let updatedBoards = updateBoardsWithNumber listOfBingoBoards number
            // Check for winner status
            let winnerCandidates = 
                updatedBoards
                |> List.filter(fun bingoBoard -> 
                    checkBingoBoardForWin bingoBoard)
            if winnerCandidates.Length > 0
            then
                ([winnerCandidates.Head], number)
            else 
                (updatedBoards, number)
    ) (bingoBoards, 0)

let winnerBoardSum = 
    fst winner
    |> List.head 
    |> getUnusedNumbers
    |> List.sum

let answer = winnerBoardSum * snd winner

printfn "Answer 1: %d"  answer

let loser = 
    bingoNumbers
    |> List.fold(fun bingoBoardsAndNumberAndStatus number ->
        let listOfBingoBoards, _, status = bingoBoardsAndNumberAndStatus
        // When the only one is left
        if status
        then 
            bingoBoardsAndNumberAndStatus
        else 
            // Update boards
            let updatedBoards = updateBoardsWithNumber listOfBingoBoards number
            // Check for winner status
            let nonWinners = 
                updatedBoards
                |> List.map(fun bingoBoard -> 
                    (bingoBoard, checkBingoBoardForWin bingoBoard))
                |> List.filter (fun filterResult -> not (snd filterResult))
                |> List.map fst
            if(nonWinners.Length = 0)
            then 
                (updatedBoards, number, true)
            else 
                (nonWinners, number, false)
    ) (bingoBoards, 0, false)

let loserBoard, lastNumber, _ = loser
let loserBoardSum = 
    loserBoard
    |> List.head 
    |> getUnusedNumbers
    |> List.sum
let answer2 = loserBoardSum * lastNumber
printfn "Answer 2: %d" answer2 


