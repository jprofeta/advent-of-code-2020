printfn "Advent of Code 2020"
printfn "Day 17: Conway Cubes"
printfn ""

open System
open System.Collections.Generic
open System.Text.RegularExpressions

#load "Day17Input.fsx"; open Input

type Coord = {
    x : int
    y : int
    z : int
    w : int
}
with
    static member getX (c : Coord) = c.x
    static member getY (c : Coord) = c.y
    static member getZ (c : Coord) = c.z
    static member getW (c : Coord) = c.w
    static member create2 (vec : int*int) =
        let (x,y) = vec
        { Coord.x = x; y = y; z = 0; w = 0 }
    static member create (vec : int*int*int) =
        let (x,y,z) = vec
        { Coord.x = x; y = y; z = z; w = 0 }
    static member create4 (vec : int*int*int*int) =
        let (x,y,z,w) = vec
        { Coord.x = x; y = y; z = z; w = w }

type PocketDim = HashSet<Coord>

let getBounds (dim : PocketDim) =
    let x = dim |> Seq.map Coord.getX |> Seq.min
    let y = dim |> Seq.map Coord.getY |> Seq.min
    let z = dim |> Seq.map Coord.getZ |> Seq.min
    let w = dim |> Seq.map Coord.getW |> Seq.min

    let x' = dim |> Seq.map Coord.getX |> Seq.max
    let y' = dim |> Seq.map Coord.getY |> Seq.max
    let z' = dim |> Seq.map Coord.getZ |> Seq.max
    let w' = dim |> Seq.map Coord.getW |> Seq.max

    ( Coord.create4 (x,y,z,w), Coord.create4 (x',y',z',w') )

let printDim (dim : PocketDim) =
    let (min, max) = dim |> getBounds

    for w in min.w..max.w do
        printfn ""
        printf "w=%d, " w
        for z in (min.z)..(max.z) do
            printfn "z=%d" z
            for y in (min.y)..(max.y) do
                for x in (min.x)..(max.x) do
                    let c = Coord.create (x,y,z)
                    if dim.Contains(c) then
                        printf "#"
                    else
                        printf "."
                printfn ""

let countActiveNeighbors (dim : PocketDim) (coord : Coord) =
    let n =
        dim
        |> Seq.where (fun c ->
            ( (coord.x - 1) <= c.x && c.x <= (coord.x + 1) )
            && ( (coord.y - 1) <= c.y && c.y <= (coord.y + 1) )
            && ( (coord.z - 1) <= c.z && c.z <= (coord.z + 1) )
            && ( (coord.w - 1) <= c.w && c.w <= (coord.w + 1) )
        ) |> Seq.length

    if dim.Contains(coord) then n - 1 else n    // Remove self

let part1 (initialState : string) =
    let cycleCount = 6
    let pdim = PocketDim()

    initialState.Split('\n')
    |> Seq.iteri (fun y s ->
        s |> Seq.iteri (fun x c ->
            match c with
            | '#' -> pdim.Add(Coord.create2 (x, y)) |> ignore
            | '.' -> ()
            | _ -> raise <| FormatException()
        )
    )

    let cycle (dim : PocketDim) : PocketDim =
        let (min, max) = dim |> getBounds
        let dim' = PocketDim()

        for z in (min.z - 1)..(max.z + 1) do
            for y in (min.y - 1)..(max.y + 1) do
                for x in (min.x - 1)..(max.x + 1) do
                    let c = Coord.create (x,y,z)
                    let neighbors = c |> countActiveNeighbors dim
                    if dim.Contains(c) then
                        match neighbors with
                        | 2 | 3 -> dim'.Add(c) |> ignore
                        | _ -> ()
                    else
                        match neighbors with
                        | 3 -> dim'.Add(c) |> ignore
                        | _ -> ()
        dim'

    let rec cycle' (n : int) (dim : PocketDim) : PocketDim =
        let dim' = dim |> cycle
        if n <= 1 then dim'
        else cycle' (n-1) dim'

    let pdim' = cycle' cycleCount pdim
    let n = pdim'.Count

    printfn "Number active cubes: %d" n
    ()

let part2 (initialState : string) =
    let cycleCount = 6
    let pdim = PocketDim()

    initialState.Split('\n')
    |> Seq.iteri (fun y s ->
        s |> Seq.iteri (fun x c ->
            match c with
            | '#' -> pdim.Add(Coord.create2 (x, y)) |> ignore
            | '.' -> ()
            | _ -> raise <| FormatException()
        )
    )

    let cycle (dim : PocketDim) : PocketDim =
        let (min, max) = dim |> getBounds
        let dim' = PocketDim()

        for w in (min.w-1)..(max.w+1) do
            for z in (min.z - 1)..(max.z + 1) do
                for y in (min.y - 1)..(max.y + 1) do
                    for x in (min.x - 1)..(max.x + 1) do
                        let c = Coord.create4 (x,y,z,w)
                        let neighbors = c |> countActiveNeighbors dim
                        if dim.Contains(c) then
                            match neighbors with
                            | 2 | 3 -> dim'.Add(c) |> ignore
                            | _ -> ()
                        else
                            match neighbors with
                            | 3 -> dim'.Add(c) |> ignore
                            | _ -> ()
        dim'

    let rec cycle' (n : int) (dim : PocketDim) : PocketDim =
        let dim' = dim |> cycle
        if n <= 1 then dim'
        else cycle' (n-1) dim'

    let pdim' = cycle' cycleCount pdim
    let n = pdim'.Count

    printfn "Number active cubes: %d" n
    ()

printfn "Part 1: Test input"
part1 T

printfn "Part 1: Puzzle input"
part1 X

printfn "Part 2: Test input"
part2 T

printfn "Part 2: Puzzle input"
part2 X
