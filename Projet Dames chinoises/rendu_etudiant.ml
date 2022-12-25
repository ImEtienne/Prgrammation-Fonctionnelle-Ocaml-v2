let dim = 3;;

type case = int * int * int

type couleur =
  | Vert
  | Jaune
  | Rouge
  | Noir
  | Bleu
  | Marron
  | Libre (*case libre du plateau*)
  | Dehors
  (*case en dehors du plateau, utile pour l'affichage*)
  | Nombre of int
  (*pour mettre des petits noms*)
  | Nom of string

let string_of_couleur x =
  match x with
  | Vert -> "Vert"
  | Jaune -> "Jaune"
  | Rouge -> "Rouge"
  | Noir -> "Noir"
  | Bleu -> "Bleu"
  | Marron -> "Marron"
  | Libre -> "Libre"
  | Dehors -> "Dehors"
  | Nombre n -> string_of_int n
  (*pour mettre des petits noms*)
  | Nom s -> s

type case_coloree = case * couleur

type configuration = case_coloree list * couleur list

let configuration_initial = ([], [ Vert; Jaune; Rouge; Noir; Bleu; Marron ])

let liste_joueurs (_, l) = l



type coup = Du of case * case | Sm of case list

let mis_a_jour_configuration _ _ = Error "To do"

let gagnant _ = Libre

(**DEBUT DE MES FONCTIONS*)

let est_dans_losange = fun ((i,j,k):case) -> 
  (i >= -2*dim && i <= 2*dim) && (j>= -dim && j <= dim) && (k >= -dim && k <=dim);;

(*let est_dans_etoile = fun ((i,j,k):case) ->
  (i + j + k = 0) && 
  ((i >= -dim) && (i <=2*dim) && (j >= -dim) && (j <=2*dim) && (k >= -dim) && (k <= 2*dim)) ||  
  (i <= dim) && (i >= -2*dim) && (j <= dim) && (j >= -2*dim) &&( ( k <= dim) && (k >= -2*dim));;*)

(*let est_dans_etoile = fun ((i,j,k):case) ->
  (i+j+k=0) && ((i >= -dim) && (i <=2*dim)) && ((j >= -dim)&& (j <=2*dim)) && ((k >= -dim) && (k <= 2*dim)) ||  
  ((i <= dim) && (i >= -2*dim)) && ((j <= dim) && (j >= -2*dim)) &&(( k <= dim) && (k >= -2*dim));;*)
let est_dans_etoile = fun ((i,j,k):case)->
  (i+j+k=0) && ((i >= -2*dim && i <= 2*dim && j >= -dim && j <= dim && k >= -dim && k <= dim)
                || (i >= -dim && i <= dim &&j >= -2*dim && j <= 2*dim && k >= -dim &&k <= dim)
                ||(i >= -dim && i <= dim && j >= -dim && j <= dim && k >= -2*dim && k <= 2*dim)) ;;
(**Fonction par defaut du prof*)
(*question 4*)
(*let quelle_couleur = fun ((i,j,k):case) -> fun (z: configuration) -> 
  let sum = i+j+k in
    if (est_dans_etoile(i,j,k) && sum = 0) then Libre
    else Dehors;;*)

let rec tourne_case = fun y -> fun((i,j,k):case) ->
  if (y = 0) then ((i,j,k):case)
  else 
    let tourneApp = ((-i, -j, -k):case) in
    tourne_case ( y - 1) tourneApp;;

(*question 6*)
let rec tourne_couleurs_aux (liste_couleurs:couleur list) coul =
  match liste_couleurs with
    []-> coul::[]
  |t::q -> t::(tourne_couleurs_aux q coul);;

let tourne_couleurs liste_couleurs = 
  match liste_couleurs with
    []-> []
  |t ::q -> tourne_couleurs_aux q t ;;

let rec tourne_pions liste_pions = 
  match liste_pions with
    []-> []
  | t::q-> (match t with
      |(case,coul)->((tourne_case 1 case),coul)::(tourne_pions q));;

let tourne_config (config:configuration) = let (case,coul)=config in 
  match config with
    ([],[]) ->([],[])
  | (liste_cases, liste_coul) ->(tourne_pions case,tourne_couleurs coul);;

(*Question 7*)
let sont_cases_voisines = fun ((x,y,z):case) -> fun ((i,j,k):case) -> 
  if est_dans_etoile (x,y,z) && est_dans_etoile (i, j, k) then  
    (((abs (x-i))=0) && ((abs (y-j))=1 ) && ((abs (z-k))=1)) || (((abs (x-i))=1) && ((abs (y-j))=0) && ((abs (z-k))=1)) || 
    (((abs (x-i))=1) && ((abs (y-j))=1) && ((abs (z-k))=0)) 
  else false ;;

(*sont_cases_voisines (0,-1,0) (1,-1,-1)*)

(*question 8*)
let rec case_dans_config (case:case) (config:configuration) = let (i,j,k) = case in 
  match config with
    (case_couleur_list, couleur_list) -> List.mem_assoc (i,j,k) case_couleur_list;;

(*case_dans_config (6,-3,-3) (([(5,-3,-3),Bleu;(1,-3,-3),Bleu]),([Bleu;]));;*)

(*question 9*)
let rec quelle_couleur (case:case) (config:configuration) = 
  if not (est_dans_etoile case) then Dehors 
  else let (x,z) = config in 
    match x with
    | [] -> Libre
    | t :: q -> let (c1,c2) = t in
      if c1 = case then c2
      else quelle_couleur case (q,z);;

(*question 10*)
let case_voisine (case:case) =  let (i,j,k) = case in
  if i = -dim *2 then (i+1, j-1, k)
  else if i = -dim*2+1 && j = dim-1 then (i,j+1,k-1)
  else if i = -dim*2+1 && k = dim-1 then (i+1,j,k-1)
  else if i = -dim*2+2 && k = dim-2 then (i,j-1,k+1)
  else if i = -dim*2+2 && k = dim-1 then (i,j-1,k+1)
  else (10,10,10);;

let rec remplir_triangle  (conf:configuration) (col:couleur) (c:case) = 
  let (case_col_lis, coul_lis) = conf in
  let (i,j,k) = c in
  if (i,j,k) = (10,10,10) then conf 
  else remplir_triangle (((i,j,k),col) :: case_col_lis,coul_lis) col (case_voisine(i,j,k));;

(*question 11*)
let rec remplir_init_aux (conf:configuration) liste =
  match liste with
    [] -> conf
  |[e] -> remplir_init_aux (tourne_config(remplir_triangle conf e ((-dim*2),dim,dim))) []
  |t :: q -> remplir_init_aux (tourne_config (remplir_triangle conf t (-2*dim,dim,dim) ) ) q ;;

let rec remplir_init l =
  let conf = ([],l) in remplir_init_aux conf l;;

let configuration_initial =
  remplir_init [ Vert ; Jaune ; Rouge ; Noir ; Bleu ; Marron ];;

(*question 12*)
let rec est_dep_unit conf cas1 cas2 =
  let (case_col_lis,coul_lis) = conf in
  if sont_cases_voisines cas1 cas2 &&
     quelle_couleur cas2 conf = Libre &&
     est_dans_losange cas2 &&
     match coul_lis with
     | [] -> false
     | t :: q -> quelle_couleur cas1 conf = t then true else false;; 

(*question 13*)
let rec fait_dep_unit_aux (conf:configuration) (cas1:case) (cas2:case) =
  let (case_col_lis,coul_lis) = conf in
  match case_col_lis with
  |[] -> []
  |(c,coul) :: q -> if c = cas1 then (cas1, Libre) :: (cas2,coul) :: fait_dep_unit_aux (q,coul_lis) cas1 cas2
    else (c,coul) :: fait_dep_unit_aux (q,coul_lis) cas1 cas2 ;;

let fait_dep_unit  (conf:configuration) (cas1:case) (cas2:case) =
  let (case_col_lis,coul_lis) = conf in
  ((fait_dep_unit_aux conf cas1 cas2 ),coul_lis : configuration);;

(**question 14*)
let mis_a_jour_configuration config cp =
  match cp with
  |(Sm([]))-> Error "déplacement impossible"
  |(Sm([c1]))->Error "déplacement impossible"
  |(Sm(c1::c2::q)) -> if not (est_dep_unit config c1 c2) then Error "ce n'est pas un déplacement unitaire" 
    else Ok (tourne_config (fait_dep_unit config c1 c2))
  |(Du(c1,c2)) -> if not (est_dep_unit config c1 c2) then Error "ce n'est pas un déplacement unitaire" 
    else Ok (tourne_config (fait_dep_unit config c1 c2));;

(*question 15*)
let add_case (c1:case)(c2:case) =
  let (i,j,k)=c1 in let (a,b,c) = c2 in
  ((i+a,j+b,k+c):case) ;;
let diff_case (c1:case) (c2:case) =
  let (i,j,k)=c1 in 
  let (a,b,c) = c2 in ((i-a,j-b,k-c):case) ;;

(*question 16*)
let parDeux (case:case) = 
  let ((i,j,k):case) = case in ((i/2,j/2,k/2):case);;
let calcul_pivot (c_:case)(c2:case) = 
  let (i,j,k) = c_ in 
  let (a,b,c) = c2 in
  if (i != a && j !=b && k !=c) then None
  else if (i = a) && ( abs(k) + abs (c)) mod 2 = 0 then Some (parDeux (add_case c_ c2))
  else if (k = c) && ( abs(j) + abs (b)) mod 2 = 0 then Some (parDeux (add_case c_ c2))
  else if (j = b) && ( abs(i) + abs (a)) mod 2 = 0 then Some (parDeux (add_case c_ c2))
  else None;;

(*question 17*)
let vec_et_dist (c_:case)(c:case) =
  let (a1, b1, c1) = c_ in
  let (a2, b2, c2) = c in
  if (a1 = a2) then 
    if (b1 < b2) && (c1 > c2) then ((0, -1, 1), (b2 + c1))
    else ((0, 1, -1), (b1 + c2))
  else if (b1 = b2) then 
    if (a1 < a2) && (c1 > c2) then ((-1,0,1), (a2 + c1))
    else ((1,0,-1), (a1 + c1))
  else if (c1 = c2) then
    if (a1 < a2) && (b1 > b2) then ((-1,1,0), (a2 + b1))
    else ((1, -1, 0), (a1 + b2))
  else ((0,0,0), 1);;

(*question 18*)
let rec est_libre_seg_aux c1 config vec test =
  let (v,d) =vec in
  if d = 1  || not test then test
  else if quelle_couleur (diff_case c1 v) config = Libre then   est_libre_seg_aux (diff_case c1 v) config (v,d-1) true
  else est_libre_seg_aux (diff_case (diff_case c1 v) v) config (v,d-1) false ;;

let est_libre_seg c1 c2 config =
  let vec = vec_et_dist c1 c2 in
  if est_libre_seg_aux c1 config vec false then false
  else true ;;

(*question 19*)
let est_saut (c1:case) (c2:case) config =
  if quelle_couleur c2 config !=Libre then false
  else
    match calcul_pivot c1 c2 with
      None -> false
    | Some c -> if quelle_couleur c config = Libre then false
      else 
      if est_libre_seg c1 c config && est_libre_seg c c2 config then true
      else false ;;

(*question 20*)
let rec last list_case = match list_case with
    [] -> ((111,111,111):case)
  | c::[] -> (c:case)
  |c::q -> last q ;;

let rec test list_case config =
  match list_case with
    []-> true 
  |c::[]-> true
  |c ::q -> if est_saut c (List.hd q) config then test q config else false;;

let est_saut_multiple list_case config =
  let (case_col_list , col_list ) = config in match col_list with
    [] -> false
  |t::q -> let col = (quelle_couleur (List.hd (list_case)) config) in
    if col=t && est_dans_losange (last list_case) && test list_case config then true
    else false;;



