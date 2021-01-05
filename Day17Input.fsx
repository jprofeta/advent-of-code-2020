module Input
let norm (s: string) = s.Trim().Replace("\r", "")

let T = norm @"
.#.
..#
###
"

let X = norm @"
...#..#.
.....##.
##..##.#
#.#.##..
#..#.###
...##.#.
#..##..#
.#.#..#.
"
