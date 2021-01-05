printfn "Advent of Code 2020"
printfn "Day 15: Rambunctious Recitation"
printfn ""

open System
open System.Collections.Generic
open System.Text.RegularExpressions

#load "Day15Input.fsx"; open Input

let play (numList : int[]) =
    for i in 0..numList.Length-1 do
        if numList.[i] = -1 then
            let lastSpoken = numList.[i - 1]
            let turnSpoken = Array.LastIndexOf(numList, lastSpoken, i - 2)    // Don't count the last spoken value
            if (turnSpoken >= 0) then
                numList.[i] <- i - (turnSpoken + 1)
            else
                numList.[i] <- 0

    numList.[numList.Length - 1]

let part1 (input : string) =
    let n = 2020
    let numList : int[] = Array.create n -1
    input.Split(',')
    |> Seq.map int
    |> Seq.iteri (fun i x -> numList.[i] <- x)

    let lastPlayed = play numList

    printfn "Last turn (%d): %d" n lastPlayed
    ()

let part2 (input : string) =
    let n = 30000000
    let numList : int[] = Array.create n -1
    input.Split(',')
    |> Seq.map int
    |> Seq.iteri (fun i x -> numList.[i] <- x)

    let lastPlayed = play numList

    printfn "Last turn (%d): %d" n lastPlayed
    ()


printfn "Part 1: Test input"
part1 T                 // Expected 436
part1 (norm @"1,3,2")   // Expected 1
part1 (norm @"2,1,3")   // Expected 10
part1 (norm @"1,2,3")   // Expected 27
part1 (norm @"2,3,1")   // Expected 78
part1 (norm @"3,2,1")   // Expected 438
part1 (norm @"3,1,2")   // Expected 1836

printfn "Part 1: Puzzle input"
part1 X

printfn "Part 2: Test input"
part2 T                 // Expected 175594
part2 (norm @"1,3,2")   // Expected 2578
part2 (norm @"2,1,3")   // Expected 3544142
part2 (norm @"1,2,3")   // Expected 261214
part2 (norm @"2,3,1")   // Expected 6895259
part2 (norm @"3,2,1")   // Expected 18
part2 (norm @"3,1,2")   // Expected 362

printfn "Part 2: Puzzle input"
part2 X


