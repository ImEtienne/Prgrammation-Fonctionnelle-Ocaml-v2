let pow7 = fun n->
  let carre = n * n in 
  let carre2 = carre * carre in
  n * carre * carre2;;
let g a b c = (a  b)b

let find_number s i =
  let app = String.get s i in
  if i > String.length s then i
  else if int_of_char app > 48 && int_of_char app  < 57 then int_of_char (String.get s i)
  else i+1;;

find_number "etien" 2;;

let g (a,b) =  b(b a)  ;;



let ex = Plus (Max (Plus (Var "x", Val 1.3), Val 31.4 ),  Max (Val 7.9, Val 4.1));;

let rec list_to_string f l =
  if l=0 then (string_of_int l)
  else f (string_of_int l) :: (list_to_string l);;

type expr =
  | Val of float
  | Var of string
  | Max of expr * expr
  | Plus of expr * expr;;

let rec string_of_expr expr = 
  match expr with
  | Val n -> n
  | Var n -> n
| Max (n1,n2) -> (match (n1,n2) with
    | (Val n1, Val n2) -> max (Val (n1,n2)))
|Plus (e1,e2) -> (match (e1,e2) with
        (Val e1, Val e2) -> Val (e1 + e2));;