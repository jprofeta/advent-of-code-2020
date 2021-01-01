printfn "Advent of Code 2020"
printfn "Day 9: Encoding Error"
printfn ""

open System
open System.Collections.Generic
open System.Text.RegularExpressions

#load "Day09Input.fsx"; open Input

let indexOfInvalid (windowSize : int) (cypherStream : int64 seq) : int =
    (cypherStream
    |> Seq.windowed (windowSize + 1)
    |> Seq.takeWhile (fun w ->
        let i = w.[windowSize]
        let p = w.[0..windowSize-1]
        p
        |> Seq.exists (fun j ->
            let k = i - j
            k >= 0L && k <> j && (p |> Seq.contains (k))
        )
    ) |> Seq.length) + windowSize  // Adjust by len to take into account windowing

let part1 (len : int) (stream : string) =
    let cypherStream = stream.Split('\n') |> Seq.map int64 |> Seq.toList
    let i = indexOfInvalid len cypherStream
    printfn "Detected invalid number: %d" cypherStream.[i]
    ()

let part2 (len : int) (stream : string) =
    let cypherStream = stream.Split('\n') |> Seq.map int64 |> Seq.toList
    let i = indexOfInvalid len cypherStream
    let n = cypherStream.[i]

    let slice =
        cypherStream
        |> Seq.take (i-1)
        |> Seq.cache

    let x =
        seq { 0..i-2 }
        |> Seq.map (fun j ->
            {| start = j;
                sums =
                    slice
                    |> Seq.skip j
                    |> Seq.scan (fun state k -> state + k) 0L |}
        ) |> Seq.where (fun x -> x.sums |> Seq.contains n)
        |> Seq.head
    let m = (x.sums |> Seq.findIndex (fun s -> s = n))
    let range = slice |> Seq.skip x.start |> Seq.take m |> Seq.cache
    let min = range |> Seq.min
    let max = range |> Seq.max
    printfn "Detected invalid number: %d" n
    printfn "Min range: %d" min
    printfn "Max range: %d" max
    printfn "Sum: %d" (min + max)
    ()

#time

printfn "Part 1: Test input"
part1 5 T

printfn "Part 1: Puzzle input"
part1 25 X

printfn "Part 2: Test input"
part2 5 T

printfn "Part 2: Puzzle input"
part2 25 X

#time
