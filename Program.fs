open System
open System.IO

// Day 0

let readLines q = 
    File.ReadAllLines(sprintf "./inputs/%s.txt" q)


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
    let rec findQuotient divisor rest =
        match List.tryFind (isDivisor divisor) rest with
        | Some m -> m / divisor
        | _ -> findQuotient rest.Head rest.Tail
    let lineCrc (line : string) =
        line.Split('\t') 
        |> Array.map (int)
        |> Array.sort
        |> List.ofArray
        |> (fun values -> findQuotient values.Head values.Tail)
    lines
    |> Seq.map lineCrc
    |> Seq.sum
    |> string
  

// Day 3

let spiralDistance input =
    let value = int input  
    let width = 
        value 
        |> float |> sqrt |> ceil 
        |> (*) 0.5 |> int |> (*) 2 |> (+) 1
    let distToEdge = width / 2
    let perimeter_count = value - ((width - 2) * (width - 2))
    let past_centre = (perimeter_count + width / 2) % (max 1 (width - 1))
    let distAroundEdge = min past_centre (width - past_centre)
    distToEdge + distAroundEdge
    |> string

let spiralSums inputText =
    let right (x, y) = (x + 1, y)
    let above (x, y) = (x, y + 1)
    let left (x, y) = (x - 1, y)
    let below (x, y) = (x, y - 1)          
    let walkPerimeter width =
        let repeat d n = seq{ for _ in 1..n do yield d}
        [
            seq{ yield right };
            repeat above (width - 2);
            repeat left (width - 1);
            repeat below (width - 1);
            repeat right (width - 1);
        ] |> Seq.concat
    let spiralMoves () =
        Seq.initInfinite ((+) 1 >> (*) 2 >> (+) 1)
        |> Seq.map walkPerimeter
        |> Seq.concat
    let home = (0, 0)
    let spiralCoords () =
        (home, (spiralMoves ()).GetEnumerator())
        |> Seq.unfold (fun (coord, moves) ->
            moves.MoveNext() |> ignore         
            Some (coord, ((moves.Current coord), moves)))                 
    let lookupValue spiral (x, y) =
        spiral
        |> List.tryFind (fun ((_x, _y), _) ->
            x = _x && y = _y)
        |> function
            | Some ((_, _), n) -> Some n
            | _ -> None             
    let sumNeighbours squares coord = 
        let neighbours =
            [ right; right>>above; above; above>>left; left; left>>below; below; below>>right ]
            |> List.map (fun f -> f coord)
        neighbours
        |> Seq.map (lookupValue squares)
        |> Seq.map (function Some n -> n | None -> 0)
        |> Seq.sum
    let first = (home, 1)     
    let spiralSquares () =
        let en = (spiralCoords ()).GetEnumerator()
        en.MoveNext() |> ignore // already have first
        ([first], en)
        |> Seq.unfold (fun (spiral, coords) ->
            coords.MoveNext() |> ignore
            let nextCoord = coords.Current
            let nextSquare = (nextCoord, sumNeighbours spiral nextCoord)  
            Some (List.head spiral, (nextSquare::spiral, coords)))
    let input = int inputText
    spiralSquares ()    
        |> Seq.map snd
        |> Seq.find ((<) input)
        |> string


 // Day 4

let validPassphrases (lines : string[]) =
    lines
    |> Seq.map (fun line -> line.Split(' '))
    |> Seq.filter(fun words -> words.Length = (Seq.distinct words |> Seq.length))
    |> Seq.length
    |> string

let anagramFree (lines : string[]) =
    lines
    |> Seq.map (fun line -> line.Split(' '))
    |> Seq.map (Array.map (Seq.toArray >> Array.sort))
    |> Seq.filter(fun text -> text.Length = (Seq.distinct text |> Seq.length))
    |> Seq.length
    |> string


// Day 5

let jumpCount lines stranger =
    let length = Array.length lines
    let getNextIndex  = 
        let instructions = Array.map int lines
        (fun index -> 
            let offset = instructions.[index]
            let next = index + offset
            let mutation = 
                match stranger, offset with
                | (true, _) when offset > 2 -> -1
                | _ -> 1
            instructions.[index] <- offset + mutation
            next)
    let rec countJumps index count =
        match 0 <= index && index < length with
        | true -> countJumps (getNextIndex index) (count + 1)
        | false -> count
    countJumps 0 0
    |> string


[<EntryPoint>]
let main argv =
    match argv with
    | [| "1a"; input |] -> inverseCaptureA input
    | [| "1b"; input |] -> inverseCaptureB input
    | [| "2a" |] -> checksumDiff (readLines "2a")
    | [| "2b" |] -> checksumQuotient (readLines "2b")
    | [| "3a"; input |] -> spiralDistance input
    | [| "3b"; input |] -> spiralSums input
    | [| "4a" |] -> validPassphrases (readLines "4a")
    | [| "4b" |] -> anagramFree (readLines "4a")
    | [| "5a" |] -> jumpCount (readLines "5a") false
    | [| "5b" |] -> jumpCount (readLines "5a") true
    | _ -> "Merry Christmas from F#!"
    |> printfn "%s"
    Console.ReadLine() |> ignore
    0

