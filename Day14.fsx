printfn "Advent of Code 2020"
printfn "Day 14: Docking Data"
printfn ""

open System
open System.Collections.Generic
open System.Text.RegularExpressions

#load "Day14Input.fsx"; open Input

let maskRegex = Regex(@"^mask\s+=\s+(?<mask>[01X]+)$", RegexOptions.Compiled)
type Mask1 = {
    Or : uint64     // Sets one bits
    And : uint64    // Sets zero bits
}
with
    member this.apply (value : uint64) = (value ||| this.Or) &&& this.And
    static member zero = { Mask1.Or = 0UL; And = ~~~0UL }
    static member parse (text : string) : Mask1 =
        let rec p (m : string) (i : int) (x : uint64*uint64) : uint64*uint64 =
            if i >= m.Length then
                x
            else
                let (obit, abit) =
                    match m.[i] with
                    | '1' -> (1UL, 0UL)
                    | '0' -> (0UL, 1UL)
                    | 'X' -> (0UL, 0UL)
                    | _ -> raise <| FormatException()
                let (xo,xa) = x
                let x' = (
                    (xo <<< 1) ||| obit,
                    (xa <<< 1) ||| abit
                )
                p m (i + 1) x'
        let rmatch = maskRegex.Match(text)
        if not (rmatch.Success) then raise <| FormatException()
        else
            let (obit, abit) = p rmatch.Groups.["mask"].Value 0 (0UL,0UL)
            { Mask1.Or = obit; And = ~~~abit }

type Mask2 = {
    Or : uint64
    Floating : int list
}
with
    member this.apply (address : uint64) =
        let addr = address ||| this.Or
        if this.Floating.Length = 0 then
            addr |> Seq.singleton
        else
            let rec permuteFloat i addr =
                let mask = 1UL <<< this.Floating.[i]
                if (i = this.Floating.Length - 1) then
                    seq [ addr ||| mask;
                          addr &&& ~~~mask ]
                else
                    let addrs = permuteFloat (i + 1) addr |> Seq.cache
                    [ addrs |> Seq.map (fun a -> a ||| mask);
                      addrs |> Seq.map (fun a -> a &&& ~~~mask) ]
                    |> Seq.concat

            permuteFloat 0 addr

    static member zero = { Mask2.Or = 0UL; Floating = [] }
    static member parse (text : string) : Mask2 =
        let rec p (m : string) (i : int) (x : uint64*list<int>) : uint64*list<int> =
            if i >= m.Length then
                x
            else
                let (mbit, floatbit) =
                    match m.[i] with
                    | '1' -> (1UL, None)
                    | '0' -> (0UL, None)
                    | 'X' -> (0UL, Some i)
                    | _ -> raise <| FormatException()
                let (xmask,xfloat) = x
                let x' = (
                    (xmask <<< 1) ||| mbit,
                    if floatbit.IsSome then (List.append xfloat [35-floatbit.Value]) else xfloat
                )
                p m (i + 1) x'
        let rmatch = maskRegex.Match(text)
        if not (rmatch.Success) then raise <| FormatException()
        else
            let (mask, floats) = p rmatch.Groups.["mask"].Value 0 (0UL,[])
            { Mask2.Or = mask; Floating = floats }

let memRegex = Regex(@"^mem\[(?<addr>[0-9]+)\]\s+\=\s+(?<value>[0-9]+)$", RegexOptions.Compiled)
type Mem = {
    addr : uint64
    value : uint64
}
with
    static member parse (text : string) : Mem =
        let m = memRegex.Match(text)
        if not (m.Success) then raise <| FormatException()
        else
            { Mem.addr = m.Groups.["addr"].Value |> uint64; value = m.Groups.["value"].Value |> uint64 }

type Instruction1 =
    | Mask of Mask1
    | Mem of Mem

type Instruction2 =
    | Mask of Mask2
    | Mem of Mem

let parseInstr1 (text : string) : Instruction1 =
    if text.StartsWith("mask") then
        Instruction1.Mask (Mask1.parse text)
    else
        Instruction1.Mem (Mem.parse text)

let parseInstr2 (text : string) : Instruction2 =
    if text.StartsWith("mask") then
        Instruction2.Mask (Mask2.parse text)
    else
        Instruction2.Mem (Mem.parse text)

type MemoryMap = Dictionary<uint64, uint64>
let part1 (programText : string) =
    let prgm = programText.Split('\n') |> Seq.map parseInstr1
    let (_, mem) =
        prgm
        |> Seq.fold (fun (state : Mask1*MemoryMap) instr ->
            let (activeMask, memory) = state
            match instr with
            | Instruction1.Mask m -> (m, memory)
            | Instruction1.Mem m-> memory.[m.addr] <- activeMask.apply m.value; state
        ) (Mask1.zero, MemoryMap())

    let sum = mem.Values |> Seq.sum
    printfn "Sum of all memory locations: %d" sum
    ()

let part2 (programText : string) =
    let prgm = programText.Split('\n') |> Seq.map parseInstr2
    let (_, mem) =
        prgm
        |> Seq.fold (fun (state : Mask2*MemoryMap) instr ->
            let (activeMask, memory) = state
            match instr with
            | Instruction2.Mask m -> (m, memory)
            | Instruction2.Mem m-> activeMask.apply m.addr |> Seq.iter (fun a -> memory.[a] <- m.value); state
        ) (Mask2.zero, MemoryMap())

    let sum = mem.Values |> Seq.sum
    printfn "Sum of all memory locations: %d" sum
    ()

printfn "Part 1: Test input"
part1 T1

printfn "Part 1: Puzzle input"
part1 X

printfn "Part 2: Test input"
part2 T2

printfn "Part 2: Puzzle input"
part2 X



