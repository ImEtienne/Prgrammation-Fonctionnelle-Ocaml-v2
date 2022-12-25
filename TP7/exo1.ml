(**question 1*)
let rec somme = fun l ->
    match l with
    | [] -> 0.
    |t::q -> t +. somme q;;
  
    somme [1.; 2.; 3.];;

(**question 2*)
let rec compte = fun l ->
    match l with
    | [] -> 0
    | t::q -> if t then 1 + compte q
        else compte q;;

  compte [true; false; true];;

  (**question 3*)
  let rec asso = fun l a -> 
    match l with 
    [] -> None
    | (t1,t2) ::q -> if t1=a then Some t2
            else asso q a;;
    
    asso [(1,4); (3,20); (3,5)] 3;;

  (*question 4*)
  let rec div2 = fun l ->
    match l with
    [] -> []
    | t::q -> if t mod 2 = 0 then t/2:: div2 q
            else [];;
      div2 [3;9]  ;;

  (**question 5*)
  let rec filter_map = fun f l ->
  match l with
  | [] -> []
  | t::q -> match f t with
      | None -> filter_map f q
      | Some l -> l::filter_map f q;;

let safe_sqrt i = if i >= 0 then Some (sqrt i) else None;;

let mylst = [4.;9.;(-3.);25.];;

(**// [2.; 3.; 5.]*);;

(**filter_map safe_sqrt mylst;;*);;