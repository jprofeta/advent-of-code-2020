printfn "Advent of Code 2020"
printfn "Day 12: Rain Risk"
printfn ""

open System
open System.Collections.Generic
open System.Text.RegularExpressions

#load "Day12Input.fsx"; open Input

type Point = {
    x: int
    y: int
}
with
    static member zero = { Point.x = 0; y = 0 }
    static member create (tuple : int*int) =
        let (x,y) = tuple
        { Point.x = x; y = y}
    static member rotate (deg : int) (p : Point) : Point = 
        let deg' = if deg < 0 then deg + 360 else deg
        match deg' with
        | 0 | 360 -> p
        | 90 -> { Point.x = -p.y; y = p.x }
        | 180 -> { Point.x = -p.x; y = -p.y }
        | 270 -> { Point.x = p.y; y = -p.x }
        | _ -> raise <| ArgumentException(sprintf "Invalid angle: %d. Only multiples of 90 deg within [-360,360] are allowed!" deg, nameof deg)
    static member manhattenDistance (p : Point) : int = Math.Abs(p.x) + Math.Abs(p.y)
    static member (+) (a: Point, b: Point) : Point = { Point.x = a.x + b.x; y = a.y + b.y }
    static member (*) (a: Point, b : int) : Point = { Point.x = a.x * b; y = a.y * b }

type NavAction = 
    | North
    | South
    | East
    | West
    | Left
    | Right
    | Forward

type NavInstruction = {
    action : NavAction
    value : int
}
with
    static member parse (instr : string) : NavInstruction =
        try
            let value = instr.Substring(1) |> int
            match instr.[0] with
            | 'N' -> { NavInstruction.action = NavAction.North;   value = value }
            | 'S' -> { NavInstruction.action = NavAction.South;   value = value }
            | 'E' -> { NavInstruction.action = NavAction.East;    value = value }
            | 'W' -> { NavInstruction.action = NavAction.West;    value = value }
            | 'L' -> { NavInstruction.action = NavAction.Left;    value = value }
            | 'R' -> { NavInstruction.action = NavAction.Right;   value = value }
            | 'F' -> { NavInstruction.action = NavAction.Forward; value = value }
            | _ -> raise <| Exception()
        with
        | _ -> raise <| FormatException("Invalid nav instruction.")

module CardinalDirections =
    let North = ( 0,  1) |> Point.create
    let South = ( 0, -1) |> Point.create
    let East =  ( 1,  0) |> Point.create
    let West =  (-1,  0) |> Point.create

type Ferry = {
    position : Point
    heading : Point
}
with
    static member zero = { Ferry.position = Point.zero; heading = CardinalDirections.East }
    static member move (vector : Point) (f : Ferry) : Ferry = { Ferry.position = f.position + vector; heading = f.heading }
    static member moveHeading (vector : Point) (f : Ferry) : Ferry = { Ferry.position = f.position; heading = f.heading + vector }
    static member rotate (deg : int) (f : Ferry) : Ferry = { Ferry.position = f.position; heading = f.heading |> Point.rotate deg }

let part1 (instructions : string) =
    let instrs = instructions.Split('\n') |> Seq.map NavInstruction.parse |> Seq.toList

    let endState =
        instrs
        |> Seq.fold (fun ferry instr ->
            ferry
            |> match instr.action with
                | NavAction.North -> Ferry.move (CardinalDirections.North * instr.value)
                | NavAction.South -> Ferry.move (CardinalDirections.South * instr.value)
                | NavAction.East -> Ferry.move (CardinalDirections.East * instr.value)
                | NavAction.West -> Ferry.move (CardinalDirections.West * instr.value)
                | NavAction.Left -> Ferry.rotate (instr.value)
                | NavAction.Right -> Ferry.rotate (-instr.value)
                | NavAction.Forward -> Ferry.move (ferry.heading * instr.value)
        ) Ferry.zero

    printfn "Ferry's final position: (%d, %d)" endState.position.x endState.position.y
    printfn "Ferry's final heading: (%d, %d)" endState.heading.x endState.heading.y
    printfn "Ferry's Manhattan distance traveled: %d" (endState.position |> Point.manhattenDistance)
    ()

let part2 (instructions : string) = 
    // In this part the `Ferry.heading` field is the waypoint (!), think of it as a direction and a stride length for the ferry.
    let instrs = instructions.Split('\n') |> Seq.map NavInstruction.parse |> Seq.toList

    let endState =
        instrs
        |> Seq.fold (fun ferry instr ->
            ferry
            |> match instr.action with
                | NavAction.North -> Ferry.moveHeading (CardinalDirections.North * instr.value)
                | NavAction.South -> Ferry.moveHeading (CardinalDirections.South * instr.value)
                | NavAction.East -> Ferry.moveHeading (CardinalDirections.East * instr.value)
                | NavAction.West -> Ferry.moveHeading (CardinalDirections.West * instr.value)
                | NavAction.Left -> Ferry.rotate (instr.value)
                | NavAction.Right -> Ferry.rotate (-instr.value)
                | NavAction.Forward -> Ferry.move (ferry.heading * instr.value)
        ) { Ferry.position = (0,0) |> Point.create; heading = (10, 1) |> Point.create }

    printfn "Ferry's final position: (%d, %d)" endState.position.x endState.position.y
    printfn "Ferry's final heading: (%d, %d)" endState.heading.x endState.heading.y
    printfn "Ferry's Manhattan distance traveled: %d" (endState.position |> Point.manhattenDistance)
    ()

printfn "Part 1: Test input"
part1 T

printfn "Part 1: Puzzle input"
part1 X

printfn "Part 2: Test input"
part2 T

printfn "Part 2: Puzzle input"
part2 X

