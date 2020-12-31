printfn "Advent of Code 2020"
printfn "Day 8: Handheld Halting"
printfn ""

open System
open System.Collections.Generic
open System.Text.RegularExpressions

#load "Day8Input.fsx"; open Input

type Instruction = string*int

let parseInstruction (str : string) : Instruction =
    let i = str.IndexOf(' ')
    (str.Substring(0, i), str.Substring(i + 1) |> int)

let part1 (program : string) =
    let prgm = program.Split('\n') |> Seq.map parseInstruction |> Seq.toArray
    let executedLine : bool[] = Array.zeroCreate(prgm.Length)

    let rec exec (prgmCtr : int) (accumulator : int) : {| acc:int; prgmCtr:int |} =
        if executedLine.[prgmCtr] then
            {| acc = accumulator; prgmCtr = prgmCtr |}
        else
            executedLine.[prgmCtr] <- true
            let (cmd,arg) = prgm.[prgmCtr]
            let (newCtr, newAcc) =
                match cmd with
                | "nop" -> (prgmCtr + 1, accumulator)
                | "jmp" -> (prgmCtr + arg, accumulator)
                | "acc" -> (prgmCtr + 1, accumulator + arg)
                | _ -> raise <| FormatException(sprintf "Invalid instruction @ %d" prgmCtr);
            exec newCtr newAcc
    let res = exec 0 0
    let (cmd,arg) = prgm.[res.prgmCtr]
    printfn "Loop detected at ADDR %d (%s %d)." res.prgmCtr cmd arg
    printfn "Accumulator: %d" res.acc
    ()

let part2 (program : string) =
    let prgm = program.Split('\n') |> Seq.map parseInstruction |> Seq.toArray

    let rec exec (prgm : Instruction[]) (prgmCtr : int) (accumulator : int) (swappedInstr : int option) (execdLines : bool[]) : {| acc:int; prgmCtr:int; swappedCtr:int option |} =
        if prgmCtr >= prgm.Length then
            {| acc = accumulator; prgmCtr = -1; swappedCtr = swappedInstr |}
        else if execdLines.[prgmCtr] then
            {| acc = accumulator; prgmCtr = prgmCtr; swappedCtr = swappedInstr |}
        else
            execdLines.[prgmCtr] <- true
            let (cmd,arg) = prgm.[prgmCtr]
            let newState =
                match cmd with
                    | "nop" -> {| ctr1 = prgmCtr + 1;
                                  ctr2 = if swappedInstr.IsNone then prgmCtr + arg else -1; // Change to jmp
                                  acc = accumulator |}
                    | "jmp" -> {| ctr1 = prgmCtr + arg;
                                  ctr2 = if swappedInstr.IsNone then prgmCtr + 1 else -1;   // Change to nop
                                  acc = accumulator |}
                    | "acc" -> {| ctr1 = prgmCtr + 1;
                                  ctr2 = -1;    // Never swap instrs on acc
                                  acc = accumulator + arg |}
                    | _ -> raise <| FormatException(sprintf "Invalid instruction @ %d" prgmCtr);
            if newState.ctr2 >= 0 then
                //printfn "> Exec swapped @ ADDR %d." prgmCtr
                // Clone the program list and swap the instruction
                let prgm' = prgm.Clone() :?> Instruction[]
                prgm'.[prgmCtr] <- match cmd with | "nop" -> ("jmp", arg) | "jmp" -> ("nop", arg) | _ -> raise <| Exception()
                // Execute with the swapped program
                let res = exec prgm' newState.ctr2 newState.acc (Some prgmCtr) (execdLines.Clone() :?> bool[])
                if res.prgmCtr < 0 then
                    //printfn "> Swap succeeded."
                    res
                else
                    //printfn "> Swap failed."
                    //printfn "> Exec normal @ ADDR %d." prgmCtr
                    exec prgm newState.ctr1 newState.acc swappedInstr execdLines
            else
                //printfn "> Exec normal @ ADDR %d." prgmCtr
                exec prgm newState.ctr1 newState.acc swappedInstr execdLines
    let res = exec prgm 0 0 None (Array.zeroCreate(prgm.Length))
    if res.prgmCtr < 0 then
        if res.swappedCtr.IsSome then
            printfn "Program completed. Swapped instruction @ ADDR %d." res.swappedCtr.Value
        else
            printfn "Program completed without any changes!?!"
    else
        printfn "Loop detected @ ADDR %d." res.prgmCtr
    printfn "Accumulator: %d" res.acc
    ()

#time
printfn "Part 1: Test input"
part1 T
#time

#time
printfn "Part 1: Puzzle input"
part1 X
#time

#time
printfn "Part 2: Test input"
part2 T
#time

#time
printfn "Part 2: Puzzle input"
part2 X
#time
