module Input
let norm (s: string) = s.Trim().Replace("\r", "")

let T = norm @"0,3,6"

let X = norm @"2,15,0,9,1,20"

