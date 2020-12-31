printfn "Advent of Code 2020"
printfn "Day 5: Binary Boarding"
printfn ""

open System
open System.Collections.Generic
open System.Text.RegularExpressions

#load "Day5Input.fsx"; open Input

let bin2int (s : string) : int =
    let rec bin2int' (s : string) (i : int) (v : int) : int =
        if i >= s.Length then
            v
        else
            let v' =
                (v <<< 1)
                ||| (match s.[i] with
                    | '0' -> 0
                    | '1' -> 1
                    | _ -> raise <| FormatException())
            bin2int' s (i + 1) v'
    bin2int' s 0 0

type Seat = {
    row : int
    column : int
}
with
    member this.seatID = this.row * 8 + this.column
    static member create (seat : string) : Seat =
        let rowStr = seat.Substring(0, 7)
        let colStr = seat.Substring(7, 3)

        let row = rowStr.Replace('F', '0').Replace('B', '1') |> bin2int
        let col = colStr.Replace('L', '0').Replace('R', '1') |> bin2int
        { Seat.row = row; column = col }

let part1 (X: string) =
    let seats =
        X.Split('\n')
        |> Seq.map Seat.create
    let maxSeatID = seats |> Seq.maxBy (fun seat -> seat.seatID)
    printfn "Max Seat ID: %d" maxSeatID.seatID

    //seats |> Seq.iter (fun seat -> printfn "row %d, column %d, seat ID %d" seat.row seat.column seat.seatID)
    ()

let part2 (X : string) =
    let seats =
        X.Split('\n')
        |> Seq.map Seat.create
    let missingSeats =
        seats
        |> Seq.sortBy (fun s -> s.seatID)
        |> Seq.pairwise
        |> Seq.map (fun (a,b) -> (a,b,b.seatID - a.seatID))
        |> Seq.where (fun (a,b,d) -> d > 1)
    if missingSeats |> Seq.length > 1 then
        printfn "ERROR: Found more than one missing seat!"
    else
        let (a,_,_) = missingSeats |> Seq.head
        printfn "Missing Seat ID: %d" (a.seatID + 1)
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
// There is no test input for this part
printfn "Part 2: Puzzle input"
part2 X
#time
