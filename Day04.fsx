printfn "Advent of Code 2020"
printfn "Day 4: Passport Processing"
printfn ""

open System
open System.Collections.Generic
open System.Text.RegularExpressions

#load "Day4Input.fsx"; open Input

// These need to match the whole string so the '^' and '$' are necessary.
let lengthRegex = Regex("^(?<len>[0-9]+)(?<unit>[a-z]+)$", RegexOptions.Compiled)
let colorHexRegex = Regex("^#[0-9a-f]{6}$", RegexOptions.Compiled)
let pidRegex = Regex("^[0-9]{9}$")

let validEyeColors = [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]

let checkYear (min: int) (max: int) (str : string) =
    try
        if not(str.Length = 4) then false
        else
            let yr = str |> int
            min <= yr && yr <= max
    with |_ -> false

let checkLength (unit: string) (min: int) (max: int) (str: string) =
    try
        let m = lengthRegex.Match(str)
        if not (m.Success) then false
        else
            let l = m.Groups.["len"].Value |> int
            let u = m.Groups.["unit"].Value

            if u <> unit then false
            else min <= l && l <= max
    with |_ -> false

type Passport = {
    fields : IDictionary<string,string>
}
with
    static member requiredFields = [
        "byr"
        "iyr"
        "eyr"
        "hgt"
        "hcl"
        "ecl"
        "pid"
    ]
    static member fieldValidators = dict [
        ( "byr", checkYear 1920 2002)
        ( "iyr", checkYear 2010 2020)
        ( "eyr", checkYear 2020 2030)
        ( "hgt", fun x -> checkLength "cm" 150 193 x || checkLength "in" 59 76 x)
        ( "hcl", fun x -> colorHexRegex.IsMatch x)
        ( "ecl", fun x -> validEyeColors |> Seq.contains x)
        ( "pid", fun x -> pidRegex.IsMatch x)
    ]
    static member parse (s : string) : Passport =
        let fields =
            s.Split(' ', '\n')
            |> Seq.map (fun f ->
                let i = f.IndexOf(':')
                ( f.Substring(0, i), f.Substring(i + 1) )
            ) |> dict
        { Passport.fields = fields }

let readBatch (batch : string) =
    batch.Split([|"\n\n"|], StringSplitOptions.None)
    |> Seq.map Passport.parse

let part1 (X : string) =
    let isValid (p : Passport) : bool =
        Passport.requiredFields
        |> Seq.forall (fun code -> p.fields.ContainsKey(code))

    let passports = readBatch X
    let n = passports |> Seq.where isValid |> Seq.length
    printfn "Valid passports: %d" n
    ()

let part2 (X: string) =
    let isValid (p : Passport) : bool =
        (Passport.requiredFields
            |> Seq.forall (fun code -> p.fields.ContainsKey(code)))
        && (p.fields
            |> Seq.forall (fun kv ->
                if Passport.fieldValidators.ContainsKey(kv.Key) then
                    Passport.fieldValidators.[kv.Key](kv.Value)
                else
                    true
            ))

    let passports = readBatch X
    let n = passports |> Seq.where isValid |> Seq.length
    printfn "Valid passports: %d" n

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
printfn "Part 2: Valid passport inputs"
part2 Tpart2Valid
#time
#time
printfn "Part 2: Invalid passport inputs"
part2 Tpart2Invalid
#time
#time
printfn "Part 2: Puzzle input"
part2 X
#time
