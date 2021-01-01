printfn "Advent of Code 2020"
printfn "Day 7: Handy Haversacks"
printfn ""

open System
open System.Collections.Generic
open System.Text.RegularExpressions

#load "Day07Input.fsx"; open Input

let bagRuleRegex = Regex(@"^(?<color>.+) bags contain (?<list>.+)\.$", RegexOptions.Compiled)
let bagListItemRegex = Regex(@"(?<count>[0-9]+) (?<color>[^,.]+) bags?", RegexOptions.Compiled)
let noBagsKey = "no other bags"

type BagRule = string*list<int*string>
type Rules = IDictionary<string, list<int*string>>
let parseBagRule (rule : string) : BagRule =
        let mRule = bagRuleRegex.Match(rule)
        if mRule.Groups.["list"].Value = noBagsKey then
            ( mRule.Groups.["color"].Value, [ ] )
        else
            let containList =
                mRule.Groups.["list"].Value.Split(',')
                |> Seq.map (fun x -> bagListItemRegex.Match(x))
                |> Seq.map (fun m -> (m.Groups.["count"].Value |> int, m.Groups.["color"].Value))
                |> Seq.toList
            ( mRule.Groups.["color"].Value, containList )

let part1 (bagRules : string) (bagColor : string) =
    let rules =
        bagRules.Split('\n')
        |> Seq.map parseBagRule
        |> dict

    let rec findForColor (rules : Rules) (targetColor : string) (currentColor : string) : bool =
        let rulesForColor = rules.[currentColor]
        if rulesForColor.Length = 0 then
            false
        else
            rulesForColor
            |> Seq.map (fun (_,c) -> c)
            |> Seq.exists (fun c ->
                if c = targetColor then true
                else findForColor rules targetColor c)

    let n = rules.Keys |> Seq.where (fun c -> findForColor rules bagColor c) |> Seq.length
    printfn "Number of bag colors: %d" n
    ()

let part2 (bagRules : string) (bagColor : string) =
    let rules =
        bagRules.Split('\n')
        |> Seq.map parseBagRule
        |> dict
    let rec countForColor (rules : Rules) (baseColor : string) : int =
        let rulesForColor = rules.[baseColor]
        if rulesForColor.Length = 0 then
            0
        else
            rulesForColor
            |> Seq.map (fun (i,c) -> i + i * countForColor rules c)
            |> Seq.sum
    let n = countForColor rules bagColor
    printfn "Number of bags: %d" n
    ()

#time
printfn "Part 1: Test input for 'shiny gold'"
part1 T1 "shiny gold"
#time

#time
printfn "Part 1: Pattern input for 'shiny gold'"
part1 X "shiny gold"
#time

#time
printfn "Part 2: Test input for 'shiny gold'"
part2 T1 "shiny gold"
#time

#time
printfn "Part 2: Test input for 'shiny gold'"
part2 T2 "shiny gold"
#time

#time
printfn "Part 2: Pattern input for 'shiny gold'"
part2 X "shiny gold"
#time
