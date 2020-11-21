open System
open System.IO


// Day 0

let readLines q = 
    File.ReadAllLines(sprintf "./inputs/%s.txt" q)

let readText q = 
    File.ReadAllText(sprintf "./inputs/%s.txt" q)

let rec transpose = function
    | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []


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

let hex (input : string) partOne =

    let steps = input.Split(',')    
    
    let vector = function
        | "n"  -> 0, 2
        | "ne" -> 1, 1
        | "se" -> 1, -1
        | "s"  -> 0, -2
        | "sw" -> -1, -1
        | "nw" -> -1, 1
        | _ -> failwith "oops"

    let vectors = Array.map vector steps

    let positions =
        ((0, 0), vectors)
        ||> Seq.mapFold (fun (posA, posB) (vecA, vecB) ->
            let position = posA + vecA, posB + vecB
            (position, position))
        |> fst

    let distance (a, b) =
        let a, b = abs a, abs b
        a + (max 0 ((b - a)/2))

    let distances = positions |> Seq.map distance

    if partOne then
        Seq.last distances
    else
        Seq.max distances
    |> string


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


// Day 17
type SLink = {Value: int; mutable Next: SLink}

let spin input partOne =
    let input = int input
    
    let createLock initialValue =
        let rec initial = {Value = initialValue; Next = initial}
        (1, initial)

    let add value lock=
        let (perimeter, ring) = lock
        let added = { Value = value; Next = ring.Next }
        ring.Next <- added;
        (perimeter + 1, added);

    let advance count lock = 
        let rec iter count ring =
            match count with
            | 0 -> ring
            | _ -> iter (count - 1) ring.Next
        let (perimeter, ring) = lock
        (perimeter, iter (count % perimeter) ring)

    let advanceZero lock = 
        let rec iter ring =
            match ring.Value with
            | 0 -> ring
            | _ -> iter ring.Next
        let (perimeter, ring) = lock
        (perimeter, iter ring)

    let lockValue (_, ring) = ring.Value 

    let advanceAndAdd adv max =
        ((createLock 0), [1..max])
        ||> List.fold (fun lock value ->
        advance adv lock 
        |> add value)

    if partOne then
        advanceAndAdd input 2017
        |> advance 1
        |> lockValue
    else 
        // slow :(
        advanceAndAdd input 50_000_000
        |> advanceZero
        |> advance 1
        |> lockValue
    |> string


// Day 18

open System.Collections.Concurrent
open System.Threading.Tasks
open System.Xml

type assembleResult = Inst of int | Done of int64 

let assemble lines (partOne : bool) = 
    
    let registers () =
        let registers = Array.zeroCreate<int64> 16
        let index (name : string) =
            name |> char |> int |> (fun n -> n - (int 'a'))
        ((fun name value -> registers.[index name] <- value)
        , (fun name -> registers.[index name]))    

    let instOfString set evalPara (inst : string) =
        let op, param1Txt, param2Txt = 
            let parts = inst.Split(' ')
            parts.[0], parts.[1], if parts.Length > 2 then parts.[2] else ""
        let getParam1, getParam2 = evalPara param1Txt, evalPara param2Txt

        match op with
        | "snd" -> (fun snd rcv i -> snd (getParam1 ()); Inst (i + 1))
        | "set" -> (fun snd rcv i -> set param1Txt (getParam2 ()); Inst (i + 1))
        | "add" -> (fun snd rcv i -> set param1Txt ((getParam1 ()) + (getParam2 ())); Inst (i + 1))
        | "mul" -> (fun snd rcv i -> set param1Txt ((getParam1 ()) * (getParam2 ())); Inst (i + 1))
        | "mod" -> (fun snd rcv i -> set param1Txt ((getParam1 ()) % (getParam2 ())); Inst (i + 1))
        | "rcv" -> (fun snd rcv i -> (rcv (fun n -> set param1Txt n) i))
        | "jgz" -> (fun snd rcv i ->
            match (getParam1 ()) with
            | n when n > 0L -> Inst (i + int (getParam2 ()))
            | _ -> Inst (i + 1) )
        | _ -> failwith (sprintf "Unknown Instruction: %s" op)  
    
    let run id snd rcv = 
        let set, get = registers ()
        set "p" (int64 id)

        let evalPara (para : string) : (unit -> int64) =
            match para with
            | "" -> (fun () -> failwith "Didn't expect to be called!")
            | p when (p.Length > 1 || (char p |> int) < 64) -> (fun () -> int64 p)
            | _ -> (fun () -> get para)
        let instructions =
            lines |> Array.map (instOfString set evalPara)
        let rec performInstruction i = 
            if (i < 0 || i >= Array.length instructions) then 0L
            else
            match instructions.[i] snd rcv i with
            | Inst i -> performInstruction i
            | Done r -> r
        performInstruction 0 

    let linkedMachines () =
        let getRcv (queue : ConcurrentQueue<int64>)  = 
            let rec iter () =
                match queue.TryDequeue() with
                | true, n -> n
                | false, _ -> Task.Delay(0).Wait(); iter ()
            (fun set instIdx -> set (iter ()); Inst (instIdx + 1))

        let for1, for0 = ConcurrentQueue<int64>(), ConcurrentQueue<int64>()

        let mutable count0, count1 = 0L, 0L       
        let snd0 = (fun n -> count0 <- count0 + 1L; for1.Enqueue n)        
        let snd1 = (fun n -> count1 <- count1 + 1L; for0.Enqueue n)

        ((fun () -> run 0 snd0 (getRcv for0)),
            (fun () -> run 1 snd1 (getRcv for1)),
            (fun () -> count0, count1))
        
    if partOne then   
        let sound, recover =
            let mutable frequency = 0L
            ((fun freq -> frequency <- freq)
            , (fun sink i -> sink frequency; Done frequency))
        run 0 sound recover |> string
    else
        let run0, run1, getCounts = linkedMachines ()
        let task0, task1 = Task.Run(run0), Task.Run(run1)

        let rec wait (t : int) prev = 
            Task.Delay(1000).Wait()
            let current = getCounts() 
            if current <> prev then wait (t + 1) current
            else snd current
        wait 0 (getCounts()) |> string
        
    
// Day 19

type Direction = Up | Down | Left | Right

let tube (lines : string[]) partOne =

    let rowCount = Array.length lines
    let colCount = Seq.length lines.[0]
    let lookup (row, col) = 
        let inNetwork (row, col) =
            row >= 0 && row < rowCount 
                && col >= 0 && col < colCount
        match inNetwork (row, col) with
        | true -> lines.[row].[col]
        | false -> ' '

    let nextCoord (row, col) dir = 
        match dir with
        | Up -> (row - 1), col
        | Down -> (row + 1), col
        | Left -> row, col - 1
        | Right -> row, col + 1

    let possDirs = function
        | Up -> [Up; Left; Right]
        | Down -> [Left; Right; Down]
        | Left -> [Left; Up; Down]
        | Right -> [Up; Down; Right]

    let chooseDir coord dir =
        let nextDir = 
            possDirs dir
            |> List.map (fun testDir ->
                let testSymbol =
                    lookup (nextCoord coord testDir) 
                match testSymbol, testDir with
                | ' ', _ -> None
                | '-', Up | '-', Down -> None
                | '|', Left | '|', Right -> None
                |  _, dir -> Some dir)
            |> List.filter (function Some _ -> true | _ -> false)
        match nextDir with
        | [] -> None
        | someDir::_ -> someDir

    let nextDir coord dir  =
        let symbol = lookup coord
        match symbol, dir with
        | '|', _ | '-', _ ->  Some dir
        | _, dir -> chooseDir coord dir
    
    let rec step (coord, dir) =
        seq{
            yield lookup coord
            let coord' = nextCoord coord dir
            let dir' = nextDir coord' dir
            match dir' with
            | None -> yield lookup coord'
            | Some d -> yield! step (coord', d)}

    let steps = step ((0, 1), Down)

    if partOne then 
        steps
        |> Seq.filter (fun chr -> 'A' <= chr && chr <= 'Z')
        |> String.Concat
    else
        steps
        |> Seq.length
        |> string


// Day 20

type Particle = {Id:int; P:int64*int64*int64; V:int64*int64*int64; A:int64*int64*int64}
let particles lines partOne =
    
    let parseLine id (line : string) =
        let parseVect (text : string) =
            let [|x; y; z|] = text.Split(',') |> Array.map int64
            (x, y, z)
        let m = Regex.Match(line, 
                    "p=<(?<vector>[^>]+)>, v=<(?<vector>[^>]+)>, a=<(?<vector>[^>]+)>")
        let vects = 
            [| for cap in m.Groups.["vector"].Captures -> cap.Value |]
            |> Array.map parseVect
        {Id = id; P = vects.[0]; V = vects.[1]; A = vects.[2]}
   
    let add (x, y, z) (x', y', z') = (x + x', y + y', z + z')

    let update p =
        let vel = add p.V p.A
        { p with P = (add p.P vel); V = vel; }
    
    let removeCollisions particles =
        particles
        |> Seq.groupBy (fun p -> p.P) 
        |> Seq.filter (snd >> Seq.length >> (=) 1)
        |> Seq.collect snd

    let magnitude (x, y, z) = (abs x) + (abs y) + (abs z)

    let particles = Seq.mapi parseLine lines |> List.ofSeq

    if partOne then
        particles
        |> Seq.groupBy (fun p -> magnitude p.A)
        |> Seq.sortBy fst
        |> Seq.head
        |> snd 
        |> Seq.exactlyOne
        |> (fun s -> s.Id)
        |> string
    else
        (Seq.ofList particles, seq{1..1000})
        ||> Seq.fold (fun ps _ -> 
            ps
            |> removeCollisions 
            |> Seq.map update )
        |> Seq.length
        |> string


// Day 21

let fractal lines partOne =
    
    let original = ".#./..#/###"

    let artOfText (text : string) =
        text.Split('/')
        |> Array.map (Seq.map id >> List.ofSeq)
        |> List.ofArray

    let rulesOfLines lines =
        let flip = List.rev
        let ruleOfText (text : string) =
            let [| input; output |] = Regex.Split(text, " => ")
            (artOfText input, artOfText output)
        let ruleVariations rule =
            (rule, [ transpose; flip; transpose; flip; transpose; flip; transpose; flip; ])
            ||> Seq.mapFold 
                (fun (iputArt, outputArt) transform -> 
                    let ruleVariation = ((transform iputArt), id outputArt)
                    (ruleVariation, ruleVariation))
            |> fst        
        lines 
        |> Seq.map (ruleOfText >> ruleVariations)
        |> Seq.collect id
        |> Map.ofSeq

    let applyRule =
        let rules = rulesOfLines lines
        (fun art -> rules.[art])

    let disemble size picture =
        List.chunkBySize size picture
        |> List.map (transpose >> (List.chunkBySize size) >> (List.map transpose))

    let assemble pictures =
        pictures
        |> List.map (transpose >> (List.map List.concat))
        |> List.collect id

    let enhance art =
        let size = if (List.length art) % 2 = 0 then 2 else 3
        disemble size art
        |> List.map (List.map applyRule)
        |> assemble
           
    let rec repeat fn state times = 
        match times with
        | 0 -> state
        | _ -> repeat fn (fn state) (times - 1)
    
    if partOne then 5 else 18
    |> repeat enhance (artOfText original) 
    |> Seq.collect id
    |> Seq.filter ((=) '#')
    |> Seq.length
    |> string


// Day 22

type Health = Clean | Weakened | Infected | Flagged
type Virus = {Pos: int * int; Dir: Direction; Colony: Map<int * int, Health>; LastTreatment : Health}

let sporifica (lines : string[]) partOne =
    //let lines = [| "..#"; "#.."; "..." |]
    
    let virusOfLines (lines : string[])  =
        let offset = Array.length lines / 2        
        let colony = 
            lines
            |> Seq.mapi (fun row line ->
                line 
                |> Seq.mapi (fun col chr -> (col, chr))
                |> Seq.filter (fun (_, chr) -> chr = '#')
                |> Seq.map (fun (col, _) ->
                    ((col - offset, offset - row), Infected)))
            |> Seq.collect id
            |> Map.ofSeq
        { Pos = (0, 0); Dir = Up; Colony = colony; LastTreatment = Clean }
    
    let turnLeft =  function
        | Up -> Left | Left -> Down | Down -> Right | Right -> Up

    let turnRight = function
        | Up -> Right | Right -> Down | Down -> Left | Left -> Up

    let reverse = function
        | Up -> Down | Down -> Up | Left -> Right | Right -> Left

    let move (x, y) = function
        | Up -> x, y + 1
        | Down -> x, y - 1
        | Left -> x - 1, y
        | Right -> x + 1, y

    let turn = function
        | Clean -> turnLeft | Weakened -> id | Infected -> turnRight | Flagged -> reverse

    let simpleTreatment = function
        | Clean -> Infected | Infected -> Clean | _ -> failwith "Didn't expect this"

    let evolvedTreatment = function
        | Clean -> Weakened | Weakened -> Infected | Infected -> Flagged | Flagged -> Clean

    let lookupHealth virus =
        match virus.Colony.TryFind virus.Pos with
        | Some h -> h
        | None -> Clean

    let advance treatment virus =
        let health = lookupHealth virus
        let newDir = (turn health) virus.Dir
        let newHealth = treatment health
        {   Pos = move virus.Pos newDir
            Dir = newDir
            Colony = 
                match newHealth with
                | Clean -> virus.Colony.Remove virus.Pos
                | _ -> virus.Colony.Add (virus.Pos, newHealth) 
            LastTreatment = newHealth }

    let progression treatment (virus : Virus) =
        virus
        |> Seq.unfold (fun v -> Some (v, advance treatment v))

    let treatment, count = 
        match partOne with
        | true -> simpleTreatment, 10000
        | false -> evolvedTreatment, 10000000

    progression treatment (virusOfLines lines)
    |> Seq.skip 1
    |> Seq.take count
    |> Seq.filter (fun v -> v.LastTreatment = Infected)
    |> Seq.length
    |> string
    

 // Day 23

let conflagrate (inputLines : string[]) partOne =
    
    let newRegisters () = 
        let array = Array.zeroCreate<int64>(8)
        ((fun (name : string) value -> array.[int (char name) - int 'a'] <- value),
            (fun (name : string)  ->  array.[int (char name) - int 'a']))

    let programOfString set evalPara (inst : string) =
        let op, param1Txt, param2Txt = 
            let parts = inst.Split(' ')
            parts.[0], parts.[1], if parts.Length > 2 then parts.[2] else ""
        let getParam1, getParam2 = evalPara param1Txt, evalPara param2Txt

        match op with
        | "set" -> (fun i -> set param1Txt (getParam2 ()); op, Inst (i + 1))
        | "sub" -> (fun i -> set param1Txt ((getParam1 ()) - (getParam2 ())); op, Inst (i + 1))
        | "mul" -> (fun i -> set param1Txt ((getParam1 ()) * (getParam2 ())); op, Inst (i + 1))
        | "jnz" -> (fun i -> 
            op, match (getParam1 ()) with
                | 0L -> Inst (i + 1)
                | _ -> Inst (i + int (getParam2 ())))
        | "mod" -> (fun i -> set param1Txt ((getParam1 ()) % (getParam2 ())); op, Inst (i + 1))
        | _ -> failwith (sprintf "Unknown Instruction: %s" op)  

    let run lines set get debug =
        set "a" (if debug then 0L else 1L)
        let evalPara (para : string) : (unit -> int64) =
            match para with
            | "" -> (fun () -> failwith "Didn't expect this!")
            | num when (num.Length > 1 || (char num < '@')) -> (fun () -> int64 num)
            | register -> (fun () -> get register)
        let instructions =
            lines |> Array.map (programOfString set evalPara)
        let rec performInstruction i = 
            seq{
                if (i < 0 || i >= Array.length instructions) then 0L
                else
                let result = instructions.[i] i
                yield result
                match snd result with
                | Inst i -> yield! performInstruction i
                | Done r -> () }
        performInstruction 0 
    
    let set, get = newRegisters ()

    if partOne then
        run inputLines set get true
        |> Seq.filter (fst >> ((=) "mul"))
        |> Seq.length
        |> string 
    else
        let optimizedText = "set b 79; set c b; jnz a 2; jnz 1 5; mul b 100; sub b -100000; 
            set c b; sub c -17000; set f 1; set d 2; set e 2; set g b; mod g d; jnz g 2; 
            jnz 1 6; sub d -1 ; set g d; sub g b; jnz g -8; jnz f 2; sub h -1; set g b; 
            sub g c; jnz g 2; jnz 1 3; sub b -17; jnz 1 -18"
        let optimized = Regex.Split(optimizedText, ";\s*")
        run optimized set get false |> Seq.length |> ignore
        get "h" |> string


// Day 24

type Bridge = { Free : int; Strength : int; Used : (int * int) list; Unused : Set<int * int> }

let bridge (lines : string []) partOne = 
    
    let portA, portB = fst, snd
    let fits pins part = portA part = pins || portB part = pins
    let matches pins parts = 
        parts
        |> Seq.filter (fits pins)
        |> List.ofSeq
    let freePort pins part = 
        if portA part = pins then portB part
        else if portB part = pins then portA part
        else failwith "Didn't expect this"
    let strength part = portA part + portB part

    let addPart bridge part =
        let b = bridge
        {   Free = freePort b.Free part
            Strength = b.Strength + strength part
            Used = part::b.Used
            Unused = b.Unused.Remove part }  

    let rec build bridge =
        seq{    
            let matches = matches bridge.Free bridge.Unused
            match matches.Length with
            | 0 -> yield bridge;
            | _ -> 
            yield!
                matches 
                |> Seq.map (addPart bridge)
                |> Seq.map build
                |> Seq.collect id}
    
    let parts (lines : string[]) =
        lines 
        |> Array.map(fun line -> 
            let parts = line.Split('/')
            (int parts.[0], int parts.[1]))
        |> Set.ofArray

    let bridges = build { Free = 0; Strength = 0; Used = []; Unused = (parts lines)}

    if partOne then 
        bridges
        |> Seq.map (fun b -> b.Strength)
        |> Seq.max
    else 
        bridges
        |> Seq.groupBy (fun b -> b.Used.Length)
        |> Seq.maxBy fst
        |> snd
        |> Seq.map (fun b -> b.Strength)
        |> Seq.max
    |> string


// Day 25

type Tape (size) = 
    let mutable cursor = 0
    let array = Array.init (2 * size) (fun _ -> false)
    member t.Read () = array.[size + cursor]
    member t.Write n = array.[size + cursor] <- n
    member t.Left n = cursor <- cursor - n
    member t.Right n = cursor <- cursor + n
    member t.Checksum () = array |> Seq.filter id |> Seq.length

type States = A | B | C | D | E | F

let turing =    

    let stateA (tape : Tape) = 
        match tape.Read () with
        | false -> tape.Write true; tape.Right 1; B
        | true -> tape.Write false; tape.Left 1; E

    let stateB (tape : Tape) = 
        match tape.Read () with
        | false -> tape.Write true; tape.Left 1; C
        | true -> tape.Write false; tape.Right 1; A

    let stateC (tape : Tape) = 
        match tape.Read () with
        | false -> tape.Write true; tape.Left 1; D
        | true -> tape.Write false; tape.Right 1; C

    let stateD (tape : Tape) = 
        match tape.Read () with
        | false -> tape.Write true; tape.Left 1; E
        | true -> tape.Write false; tape.Left 1; F

    let stateE (tape : Tape) = 
        match tape.Read () with
        | false -> tape.Write true; tape.Left 1; A
        | true -> tape.Write true; tape.Left 1; C

    let stateF (tape : Tape) = 
        match tape.Read () with
        | false -> tape.Write true; tape.Left 1; E
        | true -> tape.Write true; tape.Right 1; A

    let mapState = function
    | A -> stateA | B -> stateB | C -> stateC
    | D -> stateD | E -> stateE | F -> stateF

    let rec run (tape : Tape) state steps maxSteps =
        match steps = maxSteps with
        | true -> tape.Checksum ()
        | _  -> run tape ((mapState state) tape) (steps + 1) maxSteps

    run (new Tape(10000)) A 0 12386363
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
    | [| "11a" |] -> hex (readText "11a") true
    | [| "11b" |] -> hex (readText "11a") false
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
    | [| "17a"; input |] -> spin input true
    | [| "17b"; input |] -> spin input false
    | [| "18a" |] -> assemble (readLines "18a") true
    | [| "18b" |] -> assemble (readLines "18a") false
    | [| "19a" |] -> tube (readLines "19a") true
    | [| "19b" |] -> tube (readLines "19a") false
    | [| "20a" |] -> particles (readLines "20a") true
    | [| "20b" |] -> particles (readLines "20a") false
    | [| "21a" |] -> fractal (readLines "21a") true
    | [| "21b" |] -> fractal (readLines "21a") false
    | [| "22a" |] -> sporifica (readLines "22a") true
    | [| "22b" |] -> sporifica (readLines "22a") false
    | [| "23a" |] -> conflagrate (readLines "23a") true
    | [| "23b" |] -> conflagrate (readLines "23a") false
    | [| "24a" |] -> bridge (readLines "24a") true
    | [| "24b" |] -> bridge (readLines "24a") false
    | [| "25" |] -> turing
    | _ -> "Merry Christmas from F#!"
    |> printfn "%s"
    Console.ReadLine() |> ignore
    0

