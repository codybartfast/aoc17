open System
open System.IO

// Day 1

let inverseCaptureA input = 
    let chars = Seq.toArray input   
    [ chars; [| chars.[0] |] ]
    |> Seq.concat
    |> Seq.pairwise
    |> Seq.filter (fun (a, b) -> a = b)
    |> Seq.map fst
    |> Seq.map Char.GetNumericValue
    |> Seq.sum
    |> string

let inverseCaptureB input = 
    let chars = Seq.toArray input    
    let halfLen = chars.Length / 2
    chars
    |> Seq.take halfLen
    |> Seq.mapi (fun i c -> (c, chars.[i + halfLen]))
    |> Seq.filter (fun (a, b) -> a = b)
    |> Seq.map fst
    |> Seq.map Char.GetNumericValue
    |> Seq.sum
    |> (*) 2.0
    |> string

// Day 2

let checksumDiff (lines : string[]) =
    let lineCrc (line : string) =
        let values = line.Split('\t') |> Array.map (int)
        (Array.max values) - (Array.min values)
    lines
    |> Seq.map lineCrc
    |> Seq.sum
    |> string
 
let checksumQuotient (lines : string[]) =
    let isDivisor divisor dividend =
        dividend % divisor = 0
    let rec quotient values =
        let divisor::rest = values
        match List.tryFind (isDivisor divisor) rest with
        | Some m -> m / divisor
        | _ -> quotient rest
    let lineCrc (line : string) =
        line.Split('\t') 
        |> Array.map (int)
        |> Array.sort
        |> List.ofArray
        |> quotient
    lines
    |> Seq.map lineCrc
    |> Seq.sum
    |> string
  

[<EntryPoint>]
let main argv =
    match argv with
    | [| "1a"; input |] -> inverseCaptureA input
    | [| "1b"; input |] -> inverseCaptureB input
    | [| "2a" |] -> checksumDiff (File.ReadAllLines("./inputs/2a.txt"))
    | [| "2b" |] -> checksumQuotient (File.ReadAllLines("./inputs/2b.txt"))
    | _ -> "Merry Christmas from F#!"
    |> printfn "%s"
    Console.ReadLine() |> ignore
    0




