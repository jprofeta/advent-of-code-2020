printfn "Advent of Code 2020"
printfn "Day 11: Seating System"
printfn ""

open System
open System.Collections.Generic
open System.Text.RegularExpressions

#load "Day11Input.fsx"; open Input

type Point = {
    x: int
    y: int
}
with
    static member zero = { Point.x = 0; y = 0 }
    static member create (tuple : int*int) =
        let (x,y) = tuple
        { Point.x = x; y = y}
    static member (+) (a: Point, b: Point) : Point =
        { Point.x = a.x + b.x; y = a.y + b.y }

type Array2D =
    static member GetPoint (p : Point) (array : 'T[,]) : 'T = array.[p.y,p.x]

type Seat = bool option
type SeatMap = Seat[,]
let parseSeatMap (seatMap : string) : SeatMap =
    let rows = seatMap.Split('\n')
    let seats : SeatMap = Array2D.create rows.Length rows.[0].Length None

    // Populate the seat map
    rows
    |> Seq.iteri (fun j row ->
        row |> Seq.iteri (fun i seat ->
            match seat with
            | '.' -> ()
            | 'L' -> seats.[j,i] <- Some false
            | '#' -> seats.[j,i] <- Some true
            | _ -> raise <| FormatException("Invalid seat icon") ) )

    seats

let printMap (map : SeatMap) =
    let charMap = map |> Array2D.map (fun x -> match x with | None -> '.' | Some false -> 'L' | Some true -> '#')
    for j in 0..map.GetLength(0)-1 do
        printfn "%s" (charMap.[j,*] |> String)
    printfn ""

type CountOccupiedFn = int -> int -> SeatMap -> int
let runRounds (seats : SeatMap) (countFn : CountOccupiedFn) (countLimit : int) : {| rounds : int; finalMap : SeatMap |} =
    let round (seats : SeatMap) : {| map : SeatMap; changes : int |} =
        let mutable numChanges = 0
        let newSeats =
            seats
            |> Array2D.mapi (fun j i seat ->
                match seat with
                | None -> None // This is a floor spot, so skip
                | Some false -> // Seat is empty
                    if (seats |> countFn i j) = 0 then numChanges <- numChanges + 1; Some true
                    else Some false
                | Some true -> // Seat is occupied
                    if (seats |> countFn i j) >= countLimit then numChanges <- numChanges + 1; Some false
                    else Some true
            )
        {| map = newSeats; changes = numChanges |}

    let rec runRound' (seats : SeatMap) (numRounds : int) : {| rounds : int; finalMap : SeatMap |} =
        //printMap seats
        let seats' = round seats
        if seats'.changes = 0 then
            {| rounds = numRounds; finalMap = seats'.map |}
        else
            runRound' seats'.map (numRounds + 1)

    runRound' seats 0

let part1 (seatMap : string) =
    let seats = seatMap |> parseSeatMap

    let countAdjacent i j (map : SeatMap) =
        let (i1, i2) = ( Math.Max(i - 1, 0), Math.Min(i + 1, map.GetLength(1) - 1) )
        let (j1, j2) = ( Math.Max(j - 1, 0), Math.Min(j + 1, map.GetLength(0) - 1) )
    
        let n = 
            map.[j1..j2,i1..i2]
            |> Seq.cast<bool option>
            |> Seq.where (fun seat -> seat = Some(true))
            |> Seq.length
        if map.[j,i] = Some(true) then n - 1    // Adjust for the center position
        else n

    let stableMap = runRounds seats countAdjacent 4

    let n =
        stableMap.finalMap
        |> Seq.cast<bool option>
        |> Seq.where (fun seat -> seat = Some(true))
        |> Seq.length

    printfn "Seats stabilized on round: %d" stableMap.rounds
    printfn "Number of occuped seats: %d" n
    //printMap stableMap.finalMap
    ()

let part2 (seatMap : string) =
    let seats = seatMap |> parseSeatMap
    let vectors = [
        ( 1,  0) |> Point.create
        ( 1,  1) |> Point.create
        ( 0,  1) |> Point.create
        (-1,  1) |> Point.create
        (-1,  0) |> Point.create
        (-1, -1) |> Point.create
        ( 0, -1) |> Point.create
        ( 1, -1) |> Point.create
    ]

    let countOccupied i j (map : SeatMap) : int =
        // Gets the first seat found at the given vector 'v' or None if the edge of the map is reached.
        let rec ray (p : Point) (v : Point) (map : SeatMap) : Seat =
            let p' = p + v
            if p'.x < 0 || p'.y < 0 || p'.x >= map.GetLength(1) || p'.y >= map.GetLength(0) then
                None
            else
                match map.[p'.y,p'.x] with
                | None -> ray p' v map  // Keep ray tracing if nothing is found, else return the seat
                | Some x -> Some(x)

        vectors
        |> Seq.map (fun v -> 
            if (ray ((i,j) |> Point.create) v map) = Some(true) then true
            else false
        ) |> Seq.where (fun b -> b)
        |> Seq.length

    let stableMap = runRounds seats countOccupied 5
    let n =
        stableMap.finalMap
        |> Seq.cast<bool option>
        |> Seq.where (fun seat -> seat = Some(true))
        |> Seq.length
    
    printfn "Seats stabilized on round: %d" stableMap.rounds
    printfn "Number of occuped seats: %d" n
    //printMap stableMap.finalMap
    ()

printfn "Part 1: Test input"
part1 T

printfn "Part 1: Puzzle input"
part1 X

printfn "Part 2: Test input"
part2 T

printfn "Part 2: Puzzle input"
part2 X
