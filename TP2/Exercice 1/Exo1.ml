(*question 1*);;
let carre x = x * x;;
let pow7 n = n * (carre n) * (carre (carre (n)));;

(*question 2*);;
let xor a b = if not (a == b) then true else false;;

(*question 3*);;
let perimeter a b = 2. *. ( a +. b);;

(*question 4*);;
let arg_min2 a b = if a < b then 1 else 2;;

(*question 5*);;
let arg_min2 a b = if a < b then 1 else 2;;
let arg_min3 a b c = if  c < a && c < b then 3 else arg_min2 a b;;

(*question 6*);;
let arg_min2 a b = if a < b then 1 else 2;;
let arg_min3 a b c = if  c < a && c < b then 3 else arg_min2 a b;;
let arg_min4 a b c d = if d < a && d < b && d < c then 4 else arg_min3 a b c;;

(*question 7*);;
let scalaire x1 y1 x2 y2 = (x2 *. x1) +. (y2 *. y1);;

(*question 8*);;
let scalaire x1 y1 x2 y2 = (x2 *. x1) +. (y2 *. y1);;
scalaire 2. 2. 2. 2.;;
let proj_diag x y = scalaire x y (sqrt (2.) /. 2.) ( sqrt (2.) /. 2.) ;;


