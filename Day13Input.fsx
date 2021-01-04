module Input
let norm (s: string) = s.Trim().Replace("\r", "")

let T = norm @"
939
7,13,x,x,59,x,31,19
"

let X = norm @"
1002576
13,x,x,x,x,x,x,37,x,x,x,x,x,449,x,29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,23,x,x,x,x,x,x,x,773,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,17
"
