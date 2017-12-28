open System
open System.IO


// Day 0

let readLines q = 
    File.ReadAllLines(sprintf "./inputs/%s.txt" q)

let readText q = 
    File.ReadAllText(sprintf "./inputs/%s.txt" q)


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


// Day 6

let reallocate (inputText : string) showLoopSize =
    let redistribute =
        let memory = 
            inputText.Split(' ')
            |> Array.map int
        let hash () = String.concat ":" (memory |> Seq.map string)
        (fun () ->
            let rec spread index count =
                if count = 0 then hash () else
                let index = index % memory.Length
                memory.[index] <- memory.[index] + 1
                spread (index + 1) (count - 1)
            let index, count = 
                memory 
                |> Seq.mapi (fun i n -> i, n)
                |> Seq.maxBy snd
            memory.[index] <- 0
            spread (index + 1) count)
    let rec findRepeat (hashHistory : string list) =
        let hash = redistribute ()
        match           
            hashHistory |> Seq.mapi (fun i n -> i, n) |> Seq.tryFind (snd >> (=) hash)
            with 
            | Some (i, _) -> 
                1 + if showLoopSize then i else List.length hashHistory 
            | None -> findRepeat (hash::hashHistory)
    findRepeat []
    |> string

 
// Day 7

open System.Text.RegularExpressions
open System.Runtime.Serialization

type circusDto = {Name:string; Weight:int; Supports: string list} 
type circusProgram = {Name:string; Weight:int; CumulativeWeight:int; IsBalanced: bool; Supported: circusProgram list} 
let circusParseLine line =
        let m = 
            Regex.Match(line, 
                "^(?<name>\w+) \((?<weight>\d+)\)( ->( (?<supports>\w+),)* (?<supports>\w+))?")
        { Name = m.Groups.["name"].Value
          Weight = m.Groups.["weight"].Value |> int
          Supports = [ for c in m.Groups.["supports"].Captures -> c.Value ] }          

let circusBase inputLines = 
    let dtos = inputLines |> Array.map circusParseLine 
    let names = dtos |> Seq.map (fun r -> r.Name) |> Set.ofSeq
    let supported = dtos |> Seq.collect (fun r -> r.Supports) |> Set.ofSeq
    Set.difference names supported |> Seq.head

let circusBalance inputLines =
    let dtos = 
        inputLines 
        |> Seq.map circusParseLine 
        |> Seq.map (fun dto -> dto.Name, dto)
        |> Map.ofSeq
    let rec fromDto (dto : circusDto) =
        let supported = 
            dto.Supports
            |> List.map (fun name -> dtos.Item name)
            |> List.map fromDto      
        { Name = dto.Name
          Weight = dto.Weight
          Supported = supported
          CumulativeWeight = 
                dto.Weight + (supported |> Seq.sumBy (fun d -> d.CumulativeWeight))          
          IsBalanced  = 
                supported 
                |> Seq.distinctBy (fun p -> p.CumulativeWeight)
                |> Seq.length
                |> (>) 2 }          
    let all =  
        let bottom = fromDto <| dtos.[circusBase inputLines]
        let rec getAllSupported (prog : circusProgram) =
            seq{ yield prog
                 yield! prog.Supported |> Seq.collect getAllSupported }
        bottom |> getAllSupported
    let supporterOfTheOne = 
        all 
        |> Seq.find (fun p -> 
            (not p.IsBalanced) 
            && (p.Supported |> Seq.forall (fun s -> s.IsBalanced)))
    let [[theOne]; aPeer::_] = 
        supporterOfTheOne.Supported
        |> List.groupBy (fun p -> p.CumulativeWeight)
        |> List.map snd
        |> List.sortBy List.length
    theOne.Weight + (aPeer.CumulativeWeight - theOne.CumulativeWeight)
    |> string

 
// Day 8

type registerInsruction =
    {Reg:string; Inc:int->int->int; Val:int; CReg:string; Cond:int->int->bool; CVal:int}
let registerMax inputLines justAtEnd =
    let instruction (line : string) = 
        let fs = line.Split(' ')
        {   Reg = fs.[0] 
            Inc = match fs.[1] with "inc" -> (+) | "dec" -> (-)
            Val = int <| fs.[2]
            CReg = fs.[4]
            Cond = match fs.[5] with
                    | "<=" -> (<=)
                    | "<" -> (<)
                    | "==" -> (=)
                    | "!=" -> (<>)
                    | ">" -> (>)
                    | ">=" -> (>=)
            CVal = int <| fs.[6] }
    let exec state instruction =
        let i = instruction
        let readReg name = 
            match Map.tryFind name state with
            | Some i -> i
            | _ -> 0
        if i.Cond (readReg i.CReg) i.CVal then
            Map.add i.Reg (i.Inc (readReg i.Reg) i.Val) state
        else state
    let instructions = inputLines |> Seq.map instruction
    let registers = Map.empty<string, int>    
    let highest state = 
        state
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.max
    
    if justAtEnd then  
        highest (Seq.fold exec registers instructions)
    else 
        ((registers, 0), instructions)
        ||> Seq.fold (fun (registers, maxHigh) instruction ->
            let newState = exec registers instruction
            let newHigh = max maxHigh (highest newState)
            (newState, newHigh))
        |> snd
    |> string 


// Day 9

type StreamState = {Total:int; Depth:int; InGarbage:bool; GCount:int}
let streamScore text partOne =
    let rec score stream state =
        match stream with
        | '!'::_::tail -> score tail state  
        | '<'::tail when not state.InGarbage 
                        -> score tail {state with InGarbage = true}  
        | '>'::tail -> score tail {state with InGarbage = false}  
        | _::tail when state.InGarbage 
                        -> score tail {state with GCount = state.GCount + 1 }
        | '{'::tail -> score tail {
                        state with 
                            Total = state.Total + state.Depth + 1  
                            Depth = state.Depth + 1 }                    
        | '}'::tail -> score tail {
                        state with 
                            Depth = state.Depth - 1 }                         
        | _ ::tail -> score tail state
        | [] -> if partOne then state.Total else state.GCount

    let state = {Total = 0; Depth = 0; InGarbage = false; GCount = 0}
    let stream = [for c in text -> c]
    score stream state
    |> string


// Day 10

let knotHash (inputText : string) partOne = 

    let literalLengths  () = 
        inputText.Split(',') 
        |> Seq.map int
        |> List.ofSeq

    let binaryLengths =
        List.concat [
            inputText
            |> Seq.map int
            |> List.ofSeq;
            [17; 31; 73; 47; 23]]
        
    let reverse elements current length =
        let last = (current + length) - 1
        let moded n = n % (Array.length elements)
        for step in [0..(length / 2 - 1)] do
            let a = moded (current + step)
            let b = moded (last - step)
            let temp = elements.[a]
            elements.[a] <- elements.[b]
            elements.[b] <- temp

    let rec knot elements current skip lengths =
        let moded n = n % (Array.length elements)
        match lengths with
        | [] -> (current, skip)
        | length::tail ->
            reverse elements (moded current) (moded length)
            knot elements (current + length + skip) (skip + 1) tail
    
    let repeatKnot repCount elements lengths =
        let rec repeat rCount (current, skip) =
            match rCount with 
            | 0 -> ()
            | _ -> 
                knot elements current skip lengths
                |> repeat (rCount - 1) 
        repeat repCount (0, 0)  
        
    let xor (ns : seq<int>) = Seq.reduce (^^^) ns
  
    let dense sparse =
        sparse
        |> Seq.chunkBySize 16
        |> Seq.map (xor >> (fun n -> n.ToString("x2")))
        |> String.concat "" 

    let hash = [| 0..255 |]
    if partOne then
        knot hash 0 0 (literalLengths ())|> ignore   
        hash.[0] * hash.[1] |> string
    else
        repeatKnot 64 hash binaryLengths 
        dense hash


// Day 11

let hexEd (inputText : string) partOne =
    let path =        
        //["nw"; "nw"; "nw9oo"; "sw"]
        //["ne"; "ne"; "ne"]
        //["ne"; "ne"; "sw"; "sw"]
        //["ne"; "ne"; "s"; "s"]
        //["se"; "sw"; "se"; "sw"; "sw"]
        inputText.Split(',')

    let vector compass =
        match compass with
        | "n" -> (0, 2)
        | "ne" -> (1, 1)
        | "se" -> (1, -1)
        | "s" -> (0, -2)
        | "sw" -> (-1, -1)
        | "nw" -> (-1, 1)
        | x -> failwith (sprintf "Don't know: %s" x)

    let distance path = 
        path
        |> Seq.map vector   
        |> Seq.reduce (fun (a, b) (A, B) -> (a + A, b + B))
        |> (fun (a, b) -> (abs a), (abs b))
        |> (fun (a, b) -> a + (max 0 (b - a)) / 2)
        
    let maxDistance path =
        path
        |> List.ofSeq
        |> Seq.unfold(fun path ->
            match path with
            | [] -> None
            | _::rest -> Some((distance path), rest))
        |> Seq.max

    if partOne then 
        distance path |> string
    else
        maxDistance path |> string


// Day 12

type pipe = {Id:string; Peers:string list}
let plumb (lines : string[]) partOne = 
    let pass line =
        let m = Regex.Match(line, "(?<id>\d+) <->([ ,]+(?<peer>\d+))+")
        {   Id = m.Groups.["id"].Value
            Peers = [ for cap in m.Groups.["peer"].Captures -> cap.Value] }
    let pipes = 
        lines 
        |> Seq.map (pass >> (fun p -> (p.Id, p)))
        |> Map.ofSeq   

    let rec findGroup (walked : Set<string>) id =
        match walked.Contains id with
        | true -> walked
        | _ -> List.fold findGroup (walked.Add id) (pipes.[id].Peers)
    
    let rec findGroups (unfound : Set<string>)  = 
        seq{
            let group = findGroup Set.empty<string> unfound.MinimumElement
            let remaining = Set.difference unfound group
            yield group
            if not remaining.IsEmpty then
                yield! findGroups remaining }
            
    let all = pipes |> Map.toSeq |> Seq. map fst |> Set.ofSeq
    let groups = findGroups all
    if partOne then  
        Seq.head groups
        |> Seq.length |> string
    else
        groups
        |> Seq.length |> string


// Day 13

let firewall (lines : string[]) partOne = 
    
    let ranges = 
        lines
        |> Seq.map (fun line ->
                let parts = Regex.Split(line, ": ")
                (int parts.[0], int parts.[1]))

    let level range time = 
        let range = range - 1
        match (time / range) % 2 with
        | 0 -> time % range
        | _ -> range - (time % range)

    let caught range time =
        level range time = 0
    
    let serverity delay ranges  = 
        ranges
        |> Seq.map (fun (depth, range) ->
            if caught range (delay + depth) then            
                depth * range
            else 0)
        |> Seq.sum

    let catches delay ranges  =
        ranges
        |> Seq.map (fun (depth, range) ->
            caught range (delay + depth))
    
    if partOne then 
        serverity  0 ranges
    else 
        Seq.initInfinite id
        |> Seq.map (fun delay -> 
            (delay, 
                catches delay ranges
                |> Seq.exists id))
        |> Seq.find (snd >> (=) false)
        |> fst
    |> string


// Day 14

let defrag inputText partOne =
    let diskMap key =
        [| 0..127 |]
        |> Array.map (fun n -> knotHash (key + "-" + (string n)) false)
        |> Array.map (fun text ->
            text.ToCharArray()
            |> Array.collect (fun chr -> 
                let n = Convert.ToInt32(string chr, 16)
                Convert.ToString(n, 2).PadLeft(4, '0').ToCharArray()
                |> Array.map (Char.GetNumericValue >> int)))
    
    let rows = diskMap inputText

    let findUsedCoord rows = (Array.head rows |> Array.findIndex ((=) 1), 0)

    let getUsedNeighbours (rows : int [] []) (col, row) =
        [(0, -1); (-1, 0); (1, 0); (0, 1); ]
        |> List.map (fun (colOff, rowOff) -> (col + colOff, row + rowOff))
        |> List.filter (fun (col, row) -> 
            (col >= 0 && col < rows.[0].Length
            && row >= 0 && row < rows.Length
            && rows.[row].[col] = 1))        

    let rec zeroGroup (rows : int [] []) (col, row) =
        rows.[row].[col] <- 0
        getUsedNeighbours rows (col, row)
        |> Seq.iter (zeroGroup rows)

    if partOne then 
        rows
        |> Seq.collect id
        |> Seq.sum
        |> string
    else 
    let rec countGroups rows count =
        let rs = rows |> Array.skipWhile ((Seq.sum) >> ((=) 0))
        match rs with 
        | [||] -> count
        | _ -> 
            findUsedCoord rs |> (zeroGroup rs)
            countGroups rs (1 + count)

    countGroups rows 0
    |> string


// Day 15

let duel startA filterA startB filterB partOne =
    let [startA; filterA; startB; filterB] = 
            List.map int64 [startA; filterA; startB; filterB] 

    let generate fltr factor previous =
        previous
        |> Seq.unfold (fun previous ->
            let value = (previous * factor) % 2147483647L
            Some (value, value))
        |> Seq.filter (fun n -> n % fltr = 0L)
    
    let mask = 65535L
    let judgeSame (a, b) = (a &&& mask) = (b &&& mask)
   
    let judgeCount take seriesA seriesB =
        Seq.zip seriesA seriesB
        |> Seq.take take
        |> Seq.filter judgeSame
        |> Seq.length
 
    let seriesA fltr = generate fltr 16807L  startA
    let seriesB fltr = generate fltr 48271L  startB

    if partOne then
        judgeCount 40000000 (seriesA 1L) (seriesB 1L)
    else
        judgeCount 5000000 (seriesA filterA) (seriesB filterB)
    |> string 
        
    
// Day 17

let permute (input : string) partOne =
    let spin n (array : char[]) = 
        let len = Array.length array
        Array.copy array
            |> Seq.iteri (fun i e -> array.[(i + n) % len] <- e)
    let spinOfText text = (spin (int text))

    let exchange a b (array : char[])=
        let temp = array.[a]
        array.[a] <- array.[b]
        array.[b] <- temp
    let exchangeOfText (text : string) =
        let [|a;  b |] = Array.map int (text.Split('/'))
        (exchange a b)
        
    let partner a b (array : char[]) = 
        exchange (Array.findIndex ((=)a) array) (Array.findIndex ((=) b) array) array
    let partnerOfText (text : string) =
        let [|a;  b |] = Array.map char (text.Split('/'))
        partner a b
    
    let commandOfText (text : string) =
        let command = text.Substring(0, 1)
        let paraText = text.Substring(1)
        match command with
        | "s" -> spinOfText paraText
        | "x" -> exchangeOfText paraText
        | "p" -> partnerOfText paraText
        | _ -> failwith (sprintf "unexpected command: %s" command)
   
    let start = Array.map char [|97..112|] 
    let working = Array.copy start

    let commandTexts = input.Split(',')
    let positionalCommands =  
        commandTexts 
        |> Array.filter (fun txt -> not (txt.StartsWith("p")))
        |>  Array.map commandOfText 
    let valueCommands =  
        commandTexts 
        |> Array.filter (fun txt -> txt.StartsWith("p"))
        |>  Array.map commandOfText 
 
    let positionDance () =
        Array.map (fun cmnd -> cmnd working) positionalCommands |> ignore
    let valueDance () =
        Array.map (fun cmnd -> cmnd working) valueCommands |> ignore 
    
    let createPosTransform before after =
       let transforms =
            after
            |> Array.map (fun chr -> 
                (fun (arr : char[]) -> arr.[Array.findIndex ((=) chr) before]))
       (fun arr -> Array.map (fun t -> t arr) transforms)

    let createValTransform (before : char[]) (after : char[]) =
       let transformChar char =
            after.[Array.findIndex ((=) char) before]
       (fun arr -> Array.map transformChar arr)

    let applyTransform count transform array =
        (array, seq{1..count})
        ||> Seq.fold (fun arr _ -> transform arr) 

    let applyBillion createTransform before after =
        let transform = createTransform before after
        let trans1k = createTransform before (applyTransform 1000 transform before)
        let trans1m = createTransform before (applyTransform 1000 trans1k before)
        applyTransform 1000 trans1m 

    if partOne then
        positionDance ()
        valueDance ()
        working |> String.Concat
    else 
        positionDance ()
        let applyPositionalTrans = applyBillion createPosTransform start working

        let intermediate = Array.copy working
        
        valueDance ()
        let applyValueTrans = applyBillion createValTransform intermediate working

        start
        |> applyPositionalTrans
        |> applyValueTrans
        |> String.Concat


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
    | [| "6a"; input |] -> reallocate input false
    | [| "6b"; input |] -> reallocate input true
    | [| "7a" |] -> circusBase (readLines "7a")
    | [| "7b" |] -> circusBalance (readLines "7a")
    | [| "8a" |] -> registerMax (readLines "8a") true
    | [| "8b" |] -> registerMax (readLines "8a") false
    | [| "9a" |] -> streamScore (readText "9a") true
    | [| "9b" |] -> streamScore (readText "9a") false
    | [| "10a"; input |] -> knotHash input true
    | [| "10b"; input |] -> knotHash input false
    | [| "11a" |] -> hexEd (readText "11a") true
    | [| "11b" |] -> hexEd (readText "11a") false
    | [| "12a" |] -> plumb (readLines "12a") true
    | [| "12b" |] -> plumb (readLines "12a") false
    | [| "13a" |] -> firewall (readLines "13a") true
    | [| "13b" |] -> firewall (readLines "13a") false
    | [| "14a"; input |] -> defrag input true
    | [| "14b"; input |] -> defrag input false
    | [| "15a"; startA; filterA; startB; filterB |] -> 
        duel startA filterA startB filterB true
    | [| "15b"; startA; filterA; startB; filterB |] -> 
        duel startA filterA startB filterB false
    | [| "16a" |] -> permute (readText "16a") true
    | [| "16b" |] -> permute (readText "16a") false

    | _ -> "Merry Christmas from F#!"
    |> printfn "%s"
    Console.ReadLine() |> ignore
    0

