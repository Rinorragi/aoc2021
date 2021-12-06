open System

printfn "Advent of Code Day 6"

let lanternFishes = 
    System.IO.File.ReadAllText "./input/input_day6.txt" 
    |> fun (s:string) -> s.Split(",", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int
    |> List.ofArray

let printLanternFishPopulation (day : int) (population: int64)  =
    if(day = 18 || day = 80 || day = 256 || day % 10 = 0)
    then printfn "%s: Day %d with %d lanternfishes" (DateTime.Now.ToString "HH:mm:ss") day population

let invertLanternfishesToDayTimerList (fishesWithInitialDay : int list) = 
    [|0 .. 8|]
    |> Array.fold (fun (acc : int64[]) i -> 
        let value = fishesWithInitialDay |> List.filter ((=) i) |> List.length
        Array.set acc i value
        acc
    ) (Array.create 9 0L)
    |> List.ofArray

let lanternFishPopulation = 
    [1 .. 256]
    |> List.fold (fun (dayFishArray : int64 list) dayNumber -> 
        let breeders = dayFishArray[0]
        let newPopulation = 
            dayFishArray 
            |> List.mapi (fun i _ -> 
                match i with
                | i when i < 8 && i <> 6 -> dayFishArray[i + 1]
                | 6 -> dayFishArray[i + 1] + breeders
                | 8 -> breeders
                | _ -> raise(ArgumentOutOfRangeException(sprintf "Day timer %d not allowed" i))) 
        printLanternFishPopulation dayNumber (newPopulation |> List.sum)
        newPopulation
    ) (invertLanternfishesToDayTimerList lanternFishes)