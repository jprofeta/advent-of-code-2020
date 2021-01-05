printfn "Advent of Code 2020"
printfn "Day 16: Ticket Translation"
printfn ""

open System
open System.Collections.Generic
open System.Text.RegularExpressions

#load "Day16Input.fsx"; open Input

type Range = {
    min : int
    max : int
}

type FieldRule = {
    name : string
    validRanges : Range list
}
with
    member this.isValid (value : int) : bool = this.validRanges |> Seq.exists (fun r -> r.min <= value && value <= r.max)
    static member parse (rule : string) : FieldRule =
        let namesep = rule.IndexOf(':')
        let name = rule.Substring(0, namesep)
        let rules = rule.Substring(namesep + 1).Split([| "or" |], StringSplitOptions.None)
        let ranges =
            rules
            |> Seq.map (fun r ->
                let r' = r.Trim()
                let sep = r'.IndexOf('-')
                { Range.min = r'.Substring(0, sep) |> int; max = r'.Substring(sep + 1) |> int }
            )
        { FieldRule.name = name; validRanges = ranges |> Seq.toList }

let part1 (input : string) =
    let inputParts = input.Split([| "\n\n" |], StringSplitOptions.None)
    let rules = inputParts.[0].Split('\n') |> Seq.map FieldRule.parse |> Seq.toArray
    let myTicket = inputParts.[1].Split('\n').[1].Split(',') |> Seq.map int
    let nearbyTickets = inputParts.[2].Split('\n') |> Seq.skip 1 |> Seq.map (fun x -> x.Split(',') |> Seq.map int)

    let invalidFields =
        nearbyTickets
        |> Seq.map (fun t ->
            t
            |> Seq.map (fun r -> (r, rules |> Seq.exists (fun x -> x.isValid r ) ) )
            |> Seq.where (fun (_,b) -> not b )
        )
    let errorFields = invalidFields |> Seq.concat
    let errorRate = errorFields |> Seq.map (fun (v,_) -> v) |> Seq.sum
    printfn "Scan error rate: %d" errorRate
    ()

let part2 (input : string) =
    let inputParts = input.Split([| "\n\n" |], StringSplitOptions.None)
    let rules = inputParts.[0].Split('\n') |> Seq.map FieldRule.parse |> Seq.toArray
    let myTicket = inputParts.[1].Split('\n').[1].Split(',') |> Seq.map int |> Seq.toArray
    let nearbyTickets = inputParts.[2].Split('\n') |> Seq.skip 1 |> Seq.map (fun x -> x.Split(',') |> Seq.map int)

    let validTickets =
        nearbyTickets
        |> Seq.where (fun t ->
            t
            |> Seq.forall (fun field ->
                rules |> Seq.exists (fun x -> x.isValid field)
            )
        )
    let rulesFieldMap =
        rules
        |> Seq.map (fun r ->
            (
                r,
                validTickets
                    |> Seq.map (fun t -> t |> Seq.mapi (fun i field -> (i, r.isValid field) ) ) // Run all the ticket fields through each rule
                    |> Seq.concat                   // Collect them all up
                    |> Seq.groupBy (fun (i,_) -> i) // Group by the field index
                    |> Seq.where (fun (_, ruleResults) -> ruleResults |> Seq.forall (fun (_, v) -> v) ) // Only select the fields that are all valid
                    |> Seq.map (fun (key,_) -> key) // Get the field indexes that could relate to this rule
                    |> Seq.toList   // Store as a list for later changes
            ) // Pair the index with the rule is corresponds to
        ) |> dict |> Dictionary<FieldRule,list<int>> // Convert it to a dictionary for easier lookups later and mutations

    // Loop while there are any rules that apply to more than one index
    while rulesFieldMap.Values |> Seq.where (fun v -> v.Length > 1) |> Seq.length > 0 do
        let mappedFields =
            rulesFieldMap
            |> Seq.where (fun kv -> kv.Value.Length = 1)
            |> Seq.map (fun kv -> kv.Value.Head)
            |> Seq.cache

        // Remove mapped fields from the remaining rules
        rulesFieldMap
        |> Seq.where (fun kv -> kv.Value.Length > 1)
        |> Seq.toArray  // Convert to an array in the middle so you don't get the "enumeration changed" exception
        |> Seq.iter (fun kv -> rulesFieldMap.[kv.Key] <- kv.Value |> List.choose (fun idx -> if mappedFields |> Seq.contains idx then None else Some idx))

    let rmap = rulesFieldMap |> Seq.map (fun kv -> (kv.Value.Head, kv.Key)) |> dict

    printfn "My ticket fields:"
    for i in 1..rules.Length do
        let r = rmap.[i-1]
        printfn "%2d: %3d %-20s " i myTicket.[rulesFieldMap.[r].Head] r.name
    printfn ""

    let departureProduct =
        rules
        |> Seq.where (fun r -> r.name.StartsWith("departure"))
        |> Seq.map (fun r -> myTicket.[rulesFieldMap.[r].Head])
        |> Seq.fold (fun state value -> state * (uint64 value)) 1UL

    printfn "Departure product: %d" departureProduct

    ()

printfn "Part 1: Test input"
part1 T1

printfn "Part 1: Puzzle input"
part1 X

printfn "Part 2: Test input"
part2 T2

printfn "Part 2: Puzzle input"
part2 X

