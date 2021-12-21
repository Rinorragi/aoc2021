#time
open System

type Player = {
    Id: int
    Position : int64
    Score : int64
}

type Game = {
    Player1 : Player
    Player2 : Player
    DiceThrows : int64
}

let throwDice (round : int64) = 
    if round % 100L = 0L then 100L else round % 100L

let throwThreeDices (throws : int64) =
    let dice1 = throwDice (throws + 1L) 
    let dice2 = throwDice (throws + 2L) 
    let dice3 = throwDice (throws + 3L)
    dice1 + dice2 + dice3


let updatePlayer (player: Player) (moveAmount: int64) =
    let newPositionRaw = (player.Position + moveAmount)
    let actualPosition = if newPositionRaw % 10L = 0 then 10L else newPositionRaw % 10L
    {
        Id = player.Id
        Position = actualPosition
        Score = player.Score + actualPosition
    }

let checkGameVictoryStatus (gameState : Game) =
    let status =
        [gameState.Player1;gameState.Player2]
        |> List.sortByDescending (fun f -> f.Score)
    if status.Head.Score >= 1000
    then Some(status[1].Score * gameState.DiceThrows)
    else None 

let playTurn (gameState : Game) (player : Player) =
    let moveAmount = throwThreeDices gameState.DiceThrows
    let updatedPlayer = updatePlayer player moveAmount
    {
        Player1 = if player.Id = gameState.Player1.Id then updatedPlayer else gameState.Player1
        Player2 = if player.Id = gameState.Player2.Id then updatedPlayer else gameState.Player2
        DiceThrows = gameState.DiceThrows + 3L
    }

let rec playTheGameV1(gameState : Game) =     
    let newGameStateAfterP1 = playTurn gameState gameState.Player1
    match checkGameVictoryStatus newGameStateAfterP1 with 
    | Some(x) -> printf "Anwswer: %d" x
    | None -> 
        let newGameStateAfterP2 = playTurn newGameStateAfterP1 newGameStateAfterP1.Player2
        match checkGameVictoryStatus newGameStateAfterP2 with 
        | Some(x) -> printf "Anwswer: %d" x
        | None -> playTheGameV1 newGameStateAfterP2

let playerStartPositions = 
    System.IO.File.ReadLines "./input/input_day21.txt"
    |> List.ofSeq
    |> List.map (fun f -> f.Split(" starting position: ",StringSplitOptions.RemoveEmptyEntries)[1])
    |> List.map int32

let startGame = {
    Player1 = {
        Id = 1
        Position = playerStartPositions[0]
        Score = 0L
    }
    Player2 = {
        Id = 2
        Position =playerStartPositions[1]
        Score = 0L
    }
    DiceThrows = 0L
}

printfn "Advent of code - Day 21"
playTheGameV1 startGame |> ignore
