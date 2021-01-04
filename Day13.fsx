printfn "Advent of Code 2020"
printfn "Day 13: Shuttle Search"
printfn ""

open System
open System.Collections.Generic
open System.Text.RegularExpressions

#load "Day13Input.fsx"; open Input

let part1 (schedule : string) =
    let lines = schedule.Split('\n')
    let arrivalTime = lines.[0] |> int
    let busses = lines.[1].Split(',') |> Seq.where (fun x -> not (x = "x")) |> Seq.map int |> Seq.toList

    let (bestBus, waitTime) =
        busses
        |> Seq.map (fun b -> (b, b - (arrivalTime % b) ) )
        |> Seq.minBy (fun (b,x) -> x)
    printfn "Next departing bus: %d" bestBus
    printfn "Wait time: %d" waitTime
    printfn "Result: %d" (bestBus * waitTime)
    ()

let part2 (schedule : string) =
    let lines = schedule.Split('\n')
    //let arrivalTime = lines.[0] |> int    // Ignored for this part
    let busses = lines.[1].Split(',') |> Seq.map (fun b -> if b = "x" then None else Some(int b) ) |> Seq.toList
    let maxBus = busses |> Seq.maxBy (fun x -> if x.IsNone then 0 else x.Value) |> Option.get |> int64
    let maxBusIdx = busses |> Seq.findIndex (fun b -> b = Some(maxBus |> int) )
    let busOffset = busses |> Seq.mapi (fun i _ -> i - maxBusIdx)

    let rec findTime (factor : int64) : int64*int64 =
        let t = maxBus * factor
        let tBusses =
            busses
            |> Seq.zip busOffset
            |> Seq.map (fun (i,b) ->
                match b with
                | None -> 0L
                | Some b' -> (t + (int64 i)) % (int64 b')
            )
        if tBusses |> Seq.forall (fun x -> x = 0L) then
            (factor, t + (busOffset |> Seq.head |> int64))
        else
            findTime (factor + 1L)

    let (loops,puzzleTime) = findTime 1L
    printfn "Earliest timestamp: %d" puzzleTime
    printfn "Calculation needed %d loops." loops
    ()

printfn "Part 1: Test input"
part1 T

printfn "Part 1: Puzzle input"
part1 X

printfn "Part 2: Test input"
part2 T
part2 "0\n17,x,13,19"
part2 "0\n67,7,59,61"
part2 "0\n67,x,7,59,61"
part2 "0\n67,7,x,59,61"
part2 "0\n1789,37,47,1889"

printfn "Part 2: Puzzle input"
part2 X


let mutable j = 1
for i in 1..10 do
    while ((5*j + 1) % 3 <> 0 || (5*j + 2) % 7 <> 0) do
        j <- j + 1
    printfn "Found occurrence at %d (5*%d)" (5*j) j
    j <- j + 1

part2 "0\n7,5,3"
part2 "0\n3,5,7"

part2 "0\n5,x,3"
part2 "0\n7,x,5,3"

[1; 2; 3] |> Seq.zip [3; 5; 7] |> Seq.map (fun (a,b) -> a * b)


let getInv (a) (b) (d) =
    // a^-1 → a*a^-1 = d mod b
    let rec getInv' a b d a' =
        if b = 1 || ((a'*a) % b) = d then
            a'
        else
            getInv' a b d (a' + 1)
    getInv' a b d 1
let modInv a b = getInv a b 1

let foo (x : int[]) =
    x |> Seq.pairwise |> Seq.map (fun (x,y) -> modInv y x) |> Seq.toArray
