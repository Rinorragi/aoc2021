#time
open System

type Pixel = {
    Id : string
    Row : int
    Column : int
    PixelValue : char
    Binary : string
    BinaryValue : int
} 

let charToBit (aChar : char) =
    match aChar with 
    | '#' -> '1'
    | '.' -> '0'
    | _ -> raise(ArgumentException("Invalid char"))

let positionToBinaryChar (grid : char array array) (maxRowIndex: int) (maxColumnIndex: int) (rowIndex : int) (columnIndex : int) (initialState : char) =
    if rowIndex < 0 then charToBit initialState
    elif rowIndex > maxRowIndex then charToBit initialState
    elif columnIndex < 0 then charToBit initialState
    elif columnIndex > maxColumnIndex then charToBit initialState
    else charToBit (grid[rowIndex][columnIndex])


let pixelToBinaryValue (grid : char array array) (maxRowIndex: int) (maxColumnIndex: int) (rowIndex : int) (columnIndex : int) (initialState : char) =
    [ -1 .. 1] 
    |> List.map (fun row -> 
        [| -1 .. 1|] 
        |> Array.map (fun col -> positionToBinaryChar grid maxRowIndex maxColumnIndex (rowIndex + row) (columnIndex + col) initialState)
        |> System.String)
    |> String.concat ""

let charToPixel (grid : char array array) (maxRowIndex: int) (maxColumnIndex: int) (rowIndex : int) (columnIndex : int) (initialState : char) =
    let binaryString = pixelToBinaryValue grid maxRowIndex maxColumnIndex rowIndex columnIndex initialState
    {
        Id = sprintf "%d,%d" rowIndex columnIndex
        Row = rowIndex
        Column = columnIndex
        PixelValue = grid[rowIndex][columnIndex]
        Binary = binaryString
        BinaryValue = binaryString |> (fun s -> Convert.ToInt32(s,2))
    }

let charArrayArrayToPixelArrayArray (grid : char array array) (initialState : char) =
    let maxRowIndex = grid.Length - 1
    let maxColumnIndex = grid[0].Length - 1
    grid
    |> Array.mapi (fun rowIndex row -> 
        row |> Array.mapi (fun columnIndex value -> charToPixel grid maxRowIndex maxColumnIndex rowIndex columnIndex initialState)
    )

let stretchImage (image : Pixel array array) (initialState : char) =
    // True data seems to have pixel lit on (#) at
    // ...
    // ...
    // ...
    // And to lit off at 
    // ### 
    // ###
    // ### 
    // Handle "infinity" by enforcing pixel barrier around image each round and "play like that is the infinity"
    // End result is either infinity or within those boundaries that are expanded this way
    let initialRowLength = image.Length
    let initialColumntLength = image[0].Length
    let boundaryWidth = 3
    let rowLengthChange = boundaryWidth * 2
    [|0 .. initialRowLength + rowLengthChange - 1|]
    |> Array.map (fun x -> 
        Array.init (initialColumntLength + rowLengthChange) (fun y -> 
            if (x < boundaryWidth 
                || x > (initialRowLength + boundaryWidth - 1) 
                || y < boundaryWidth 
                || y > (initialColumntLength + boundaryWidth - 1))
            then initialState
            else (image[x - boundaryWidth][y - boundaryWidth]).PixelValue))
    |> (fun f -> charArrayArrayToPixelArrayArray f initialState)

let createOutputImage (image : Pixel array array) (enhancementAlgorithm: string) (initialState : char) = 
    image
    |> Array.map (fun x -> 
        x |> Array.map (fun y -> enhancementAlgorithm[(image[y.Row][y.Column]).BinaryValue]))
    |> (fun f -> charArrayArrayToPixelArrayArray f initialState)

printfn "Advent of Code Day 20"
let nl = "\n"
let imageScannerRawData = 
    System.IO.File.ReadAllText "./input/input_day20.txt" 
    |> (fun s -> s.Split(nl + nl, StringSplitOptions.RemoveEmptyEntries))

let inputImageRaw = 
    imageScannerRawData[1] 
    |> (fun s -> s.Split(nl, StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun s -> s.ToCharArray())

let imageEnhancementAlgorithm = imageScannerRawData[0]
let inputImage = 
    charArrayArrayToPixelArrayArray inputImageRaw '.' 
    |> (fun f -> stretchImage f '.')

let litResult = 
    [1..50]
    |> List.fold (fun acc currentRound ->
            // Stupid hack to handle "infinity"
        let initialState = 
            if imageEnhancementAlgorithm[0] = '.'
            then '.'
            elif (imageEnhancementAlgorithm[0] = '#' && currentRound % 2 = 1)
            then '#' // first round . -> #
            elif imageEnhancementAlgorithm[imageEnhancementAlgorithm.Length - 1] = '.'  // second round # -> 
            then '.'
            else '#'
        let midResult = createOutputImage acc imageEnhancementAlgorithm initialState
        let largerPicture = stretchImage midResult initialState
        if currentRound = 2 || currentRound = 50
        then 
            largerPicture 
            |> Array.map(fun row -> row |> Array.filter(fun s -> s.PixelValue = '#'))
            |> Array.concat
            |> Array.length
            |> (fun f -> printfn "Answer at round %d: %d" currentRound f)
            |> ignore
        largerPicture
        
    ) inputImage

