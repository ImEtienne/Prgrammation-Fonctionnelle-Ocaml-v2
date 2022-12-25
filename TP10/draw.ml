include DrawingType

let simple_triangle =
  let pi23 = 2.0 *. pi /. 3.0 in
  Forward 25.0 :: Turn pi23 ::
  Forward 25.0 :: Turn pi23 ::
  Forward 25.0 :: []

let draw = fun _ -> simple_triangle

let inverse geom =
  match geom with
    Forward x -> Forward (-. x)
  | Turn x -> Turn (-. x)
  | Text t -> Text t;;

let apply_draw i f l =  i :: f (inverse i ::l);;


(**Solution du prof*);;
let apply_draw i f l=
  let l2 = (inverse i) :: l in
  let l3 = f l2 in
  i:: l3;;


let dragon_right n d l =
  if n =1 then (Forward d) ::  l
  else 
    let l2 = apply_draw(Turn (-.pi4)) (dragon_left (n-1) d2) l in
    apply_draw (Turn (pi4)) (dragon_right (n-1) d2) l2
and 

  let dragon_left n  d l =
    let d2 = sqrt 2.0 /. 2.0 *. d in 
    if n <=1 then (Forward d) ::  l
    else 
      let l2 = apply_draw(Turn (-.pi4)) (dragon_left (n-1) d2) l in
      apply_draw (Turn (pi4)) (dragon_right (n-1) d2) l2

type 'a arbre = Feuille | Noeud of 'a arbre * 'a * 'a arbre

let rec tree_of_list_comb l = match l with

  | [] -> Feuille
  
  | t::q -> Noeud


