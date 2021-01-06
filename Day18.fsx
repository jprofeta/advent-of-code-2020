printfn "Advent of Code 2020"
printfn "Day 18: Operation Order"
printfn ""

open System
open System.Collections.Generic
open System.Text.RegularExpressions

#load "Day18Input.fsx"; open Input

let eval (exp : string) : int =
    // This assumes the expression is well formatted!
    let rec eval' (exp : string) (i : int) : {| n:int; v:int |} =
        ()


    eval' (exp.Replace(" ", "")) 0

let part1 () =
    ()

let part2 () =
    ()


printfn "Part 1: Test input"
//part1 T

printfn "Part 1: Puzzle input"
//part1 X

printfn "Part 2: Test input"
//part2 T

printfn "Part 2: Puzzle input"
//part2 X

