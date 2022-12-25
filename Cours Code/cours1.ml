let rec fact = function
  | x when x <= 1 -> 1
  | x  -> x * fact (x - 1);;

fact 5;;

let rec fact2 = fun n ->
  if n <= 1 then 1
  else n * fact2 (n -1);;

(*let rec boucle = fun f -> fun i -> fun x ->
  if i < 10 then boucle ( i +1) (f i x )
  else x in boucle 0;;*)
1;;
(string_of_float 2.) ^" "^ "Upec";;
(1.0 +. 2.) *. (2. +. 1.) ;;
1.0 +. 2.0;;
let f x = x + 1;;

f 2;;

fact2 5;;

"upec" ^" "^ (string_of_int 2);;

let fn  f = ( f 3) *. 1.0;; (* (f(3)*)
fn ( fun f->0.);;


let fn2 f = (fun g -> g 2 f 3);;
fn2 (fun f g -> f 3);;

let f = fun x -> fun y ->
  let m = (x - y) in 
  (m * m) + m;;
(*(x - y) * (x - y) + ( x- y)*)

f 2 5;;


let plus_grand_un = fun n m -> n > m + 1 ;;
plus_grand_un 5 5;;

let string_of_val b n x =
  if b then  (string_of_int n)
  else  (string_of_float x);;

string_of_val false 234 32.;;
let app = fun f2 -> fun x -> f 2;;
app f 8;;

let rec f = fun x -> fun y ->
  if x =0 then y^" fin "
  else
    let y1 = "In"^( string_of_int x )^" ->"^ y in
    let y2 = f (x -1) y1 in
    ( y2 ^" ->Out "^( string_of_int x ));;
f 3 "";;

let carre x = x * x;;
let app = carre 5 in app;;

let ajoute_num x y = x ^ " - " ^ (string_of_int y);;
let app2 = ajoute_num "Upec" 3 in app2;;

let x = 1::2::3::4::5::[];;

let rec tete x n = 
  match x with
    [] -> 0
  |t::q -> tete x (n-1);;
tete [1] 10;;

let myList = function
  | None -> "La liste est vide"
  |Some i -> "La valeur de la lsite est "^(string_of_int i);;

let myList2 f =
  match f with
  | None ->"La liste est vide"
  | Some f -> "La valeur de la lsite est "^(string_of_int f);;
let x = 4;;
myList Some (fun x-> x);;

let estVide x = 
  match x with
    [] -> true
  |t::q -> false;;

estVide [2];;

let rec sum x =
  match x with
    [] -> 0
  |t::q -> t + sum q;;

sum [1;2;3;4;5];;
let rec length x =
  match x with
    [] -> 0
  |t::q -> 1 + length q;;

length [1; 3; 4; 5;6];;

let rec concatener list1 list2 =
  match list2 with (*inverse*)
    [] -> list1
  |t::q -> t:: concatener q list1;;

concatener  [6;7;8;9;10]  [0;1;2;3;4;5];;

let singleton = fun y -> y ::[];;

singleton 100;;

let positif = fun n ->
  if n < 0 then []
  else [n];;

let positif2 n =
  match n with
  | 0 -> []
  |_-> [n];;

positif2 (-1);;

let rec rebour = fun n->
  if n <= 0 then [0]
  else n::rebour(n-1);;

let rec rebour2 = fun n->
  match n with
  | 0 -> [0]
  |_-> n::rebour(n-1);;
rebour2 5;;

let rec compteur = fun i n ->
  if i=n then [n]
  else i::compteur (i+1) n;;

let rec compteur2 i n =
  match i with
  | n -> [n]      
  | _::_ -> i::compteur2(i+1) n;;
compteur2 1 5;;

let head = function
    [] -> -1
  |t::q -> t;;

head [3;4];;

let tail = function
  | [] -> []
  | t::q -> q;;
tail [1;2;3;4;5];;

let head_opt = function
  |[] -> None
  |t::q -> Some t;;
head_opt [];;

let tail_opt = function
  |[] -> None
  |t::q -> Some q;;

tail_opt [1;2;3;4;5];;

let rec cmp_firsts_aux f i n =
  if i=n then [f n]
  else (f i) ::cmp_firsts_aux f (i+1) (f n);;

let cmp_firsts i f x = if x = 0 then []
  else cmp_firsts_aux i f x;;

cmp_firsts f 1 10;;

(*========================TD noté 1===================*)
let rec carre_list = fun i ->
  match i with
  | [] -> 0. ::[]
  | t:: q -> (t*.t) :: (carre_list q);;

let rec perm n k =
  if k> n then 0
  else if k=0 || n = k then 1
  else n * perm(n-1) k;;

perm 5 1;;

let safe_first_char s =
  if String.length s = 0 then None
  else Some (String.get s 0);;

safe_first_char "athy" ;;

let f (a,b) c =  c b;;

let b f a = 
  if a != None then Some (f a)
  else None;;

let rec fix_point = fun f -> fun a ->
  if f a = a then a
  else fix_point f a;;
let f = fun x -> 2;;
fix_point f 3;;

let rec find f n =
  if (f n) = true then Some n
  else if n >= 0 then find f(n-1)
  else None;;

let f x = x * x;;

find f true;;




let ftostring = fun i ->
  if  (i mod 2) = 0 && (i mod 3) = 0 then "upecfst" 
  else if (i mod 3) = 0 then "fst"
  else if (i mod 2) = 0 then "upec"
  else (string_of_int i);;

ftostring 27;;

let nun = fun (x1, y1) -> fun (x2, y2) ->
  max ((abs(x2 - x1))) ((abs(y2 - y1)));;

nun (2,5) (3,2);;

let rec print_perm i n =
  if i = n then [n]
  else n::print_perm i (n-1);;

print_perm 1 3;;

let rec loop



