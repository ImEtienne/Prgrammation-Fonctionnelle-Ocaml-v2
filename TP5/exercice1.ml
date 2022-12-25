(**question 1*);;
let singleton= fun y -> [y];;

(**question 2*);;
let positif = fun n -> 
    if n < 0 then []
    else [n];;

(**question 3*);;
  let rec rebour = fun n ->
    if n = 0 then [0]
    else n::rebour(n-1);;

(**question 4*);;
let rec compteur_aux = fun i -> fun n ->
    if i=n then [n]
    else i::(compteur_aux (i+1) n);;

let compteur = fun x -> if x <= 0 then []
                else  compteur_aux 1 x;;

(**question 5*);;
let head = fun t ->
        match t with
    | [] -> -1
    | t::q -> t;;

(**question 6*);;
let tail = fun t ->
    match t with
    | [] -> []
    | t::q -> q;;

(**question 7*);;
let head_opt = fun a ->
    match a with
    | [] -> None
    | a::q -> Some a;;

(**question 8*);;
let tail_opt = fun t ->
    match t with
    | [] -> None
    | t::q -> Some q;;

(**question 9*);;
let rec cmp_firsts_aux = fun i-> fun f -> fun n->
        if i=n then [f n]
        else 
            (f i) :: cmp_firsts_aux (i + 1) f n;;

let cmp_firsts = fun f -> fun n ->  if n <=0 then [] else 
            cmp_firsts_aux 1 f  n ;;


(**question 10*);;
let rec cmp_firsts_aux = fun i-> fun f -> fun n->
        if i=n then [f n]
        else 
            (f i) :: cmp_firsts_aux (i + 1) f n;;

let cmp_firsts = fun f -> fun n ->  if n <=0 then [] else 
            cmp_firsts_aux 1 f  n ;;

let rec compteur_aux = fun i -> fun n ->
    if i=n then [n]
    else i :: (compteur_aux (i+1) n);;

let compteur = fun x -> if x <= 0 then []
                else  compteur_aux 1 x;;
            
let cmp_all = fun n -> cmp_firsts compteur(n);;

    
    
                
    

