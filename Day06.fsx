printfn "Advent of Code 2020"
printfn "Day 6: Custom Customs"
printfn ""

open System
open System.Collections.Generic
open System.Text.RegularExpressions

#load "Day6Input.fsx"; open Input

// Returns a sequence of elements that occur in all input sequences
let join (sources : seq<#seq<'T>>) : seq<'T> =
    let uniques = sources |> Seq.concat |> Seq.distinct
    uniques
    |> Seq.where (fun u ->
        sources
        |> Seq.forall (fun s -> s |> Seq.contains u)
    )

let part1 (X : string) =
    let groups = X.Split([|"\n\n"|], StringSplitOptions.None)
    let countsPerGroup =
        groups
        |> Seq.map (fun g -> Regex.Replace(g, "\s+", ""))
        |> Seq.map (fun g -> g |> Seq.distinct |> Seq.length)
    printfn "Sum of customs counts: %d" (countsPerGroup |> Seq.sum)
    ()

let part2 (X : string) =
    let groups = X.Split([|"\n\n"|], StringSplitOptions.None)
    let countsPerGroup =
        groups |> Seq.map (fun g -> g.Split('\n') |> join |> Seq.length)
    printfn "Sum of customs counts: %d" (countsPerGroup |> Seq.sum)
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
