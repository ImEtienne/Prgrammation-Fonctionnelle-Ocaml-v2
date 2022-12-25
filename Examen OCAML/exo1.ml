(*exercice 1*)
true;;
22;;
0.4;;
'c';;
Some 'c';;

let myFun  x y = x > y;;
true,"etienne";;
true :: [];;
Some 'c'::[] ;;


let bol true = (2, 'c');;

(*Exercice 2*)

let f = fun x -> x *. 5.5;;

let g x y = x ^ ( string_of_int y);;

let h x = fun y -> x -1, string_of_float y;;

let i x  = match x with (y,z) -> z *. 5.5;;

let j x y = x (2 * y);;

let k x = match x with Some y->y | None -> "a";;

let rec grow n r = if n < 0 then  r else grow (n-2) (r+ (n/2));;

#trace grow;;

grow 7 3 ;;

3-2;;


let rec fill s n = if (String.length s) = n then s
else if ((String.length s) > n) then s
else "!" ^ fill s (n-1);;


fill "abc" 10;;



