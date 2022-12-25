(*let pi = 3.14;;
let double = fun x -> x + x;;

double 13;;

let x = double 13 in double x;;
(if true then 3 else 4);;

let rec factorielle = fun x -> 
if x <= 1 then 1
else x* (factorielle(x-1));;*);;

let carre x = x *. x;;

carre 2.;;

let ajout_num x y =  x ^ "_" ^ string_of_int y;;

(ajout_num "graddy" 3);;

let distance x1 y1 x2 y2 = sqrt(carre(x2 -. x1) +. carre (y2 -. y1));;

distance 2. 4. 4. 2.;;

let appartientDisque x y r = if distance x y 0. 0. < r then true else false;;

appartientDisque 2. 2. 10.;;

(** #trace permet de decomposer l'algorithme par étape.
#untrace permet de désactiver cette fonctionnalité.*);;

let sgn = fun x -> if x >= 1. then 1 else if x = 0. then 0 else -1;;
let abs = fun y -> y * sgn (float_of_int y);;