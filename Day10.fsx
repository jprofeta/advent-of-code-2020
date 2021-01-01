printfn "Advent of Code 2020"
printfn "Day 10: Adapter Array"
printfn ""

open System
open System.Collections.Generic
open System.Text.RegularExpressions

#load "Day10Input.fsx"; open Input

let part1 (adapterList : string) =
    let a' = adapterList.Split('\n') |> Seq.map int
    let adapters = a' |> Seq.append (seq {0; (a' |> Seq.max) + 3 }) // Add the seat 'joltage' and the device's 'joltage' adapter
    let deltas =
        adapters
        |> Seq.sort
        |> Seq.pairwise
        |> Seq.map (fun (a,b) -> b-a)
    let counts =
        deltas
        |> Seq.groupBy (fun d -> d)
        |> Seq.map (fun (k,d) -> (k, d |> Seq.length))
        |> dict
    printfn "1-jolt deltas: %d" counts.[1]
    printfn "3-jolt deltas: %d" counts.[3]
    printfn "Output result: %d" (counts.[1] * counts.[3])
    ()

type Permutation = IDictionary<int,seq<int>>
let part2 (adapterList : string) =
    let a' = adapterList.Split('\n') |> Seq.map int
    let adapters =
        a'
        |> Seq.append (seq {0; (a' |> Seq.max) + 3 }) // Add the seat 'joltage' and the device's 'joltage' adapter
        |> Seq.sort
    
    let permutations =
        Seq.append adapters [-1; -1; -1]
        |> Seq.windowed 4
        |> Seq.map (fun w -> 
            let h = w |> Seq.head
            let d = w |> Seq.skip 1 |> Seq.where (fun x -> x >= 0 && (x - h) <= 3)
            (h, d) )
        |> dict

    let permCounts = Dictionary<int,int64>()
    permCounts.[adapters |> Seq.max] <- 1L  // Prefil the last adapter to 1 since it is always used

    adapters
    |> Seq.rev |> Seq.skip 1
    |> Seq.iter (fun a ->
        permCounts.[a] <-
            permutations.[a]
            |> Seq.map (fun p -> permCounts.[p])
            |> Seq.sum)
    
    printfn "Total permutations: %d" permCounts.[0]
    ()

printfn "Part 1: Test input 1"
part1 T1
printfn "Part 1: Test input 2"
part1 T2

printfn "Part 1: Puzzle input"
part1 X

printfn "Part 2: Test input 1"
part2 T1

printfn "Part 2: Test input 2"
part2 T2

printfn "Part 2: Puzzle input"
part2 X

