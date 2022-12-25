(*String.length renvoi la taille d'une chaine de caractere*);;
(*String.get renvoi le caractere a une position donné dans une chaine de caractere*);;
(*Exercice 1 : question 1*);;
let rec puissance = fun x -> fun i ->  
    if i = 0 then 1.0 
        else if i > 0 then  x *. puissance x (i-1)
        else 1. /.  x *. puissance x (i+1);;

(*question 2*);;
let rec factorial n = if n = 1 then 1
                    else n * factorial(n -1);;

(*question 3*);;
let rec stars n = if n =0 then "" 
                else "*" ^ stars(n-1);;

(*question 4*);;
let rec  compte_a_rebour n = if n=0 then (string_of_int n) 
                        else (string_of_int n)^ " "^ compte_a_rebour(n-1) ;;

(*question 5*);;
let rec compteur n = if n=1 then (string_of_int n) 
                    else compteur(n-1)^ " " ^(string_of_int n);;

(*question 6*);;
let rec sum_firsts n = if n=0 then 0 else n + sum_firsts(n-1);;                 

(*question 7*);;
let rec fsum_firsts f n = if n = 0 then 0 
            else f n + fsum_firsts f (n-1);;

(*question 8*);;
let rec fcat_firsts f n = if n=0 then ""
        else  fcat_firsts f (n -1) ^ (f n);;

(*question 9*);;
let rec fprint_firsts f n =  
        if n = 0 then ""  
        else   fprint_firsts f (n-1)^ " "^( string_of_int (f n));;

(*exercice 2 : question 10*);;
let rec retourne_aux n s = if n=0 then Char.escaped (String.get s n) 
    else  Char.escaped (String.get s n)^ (retourne_aux( n-1)s);;

(*question 11*);;
let rec retourne_aux n s = 
        if n=0 then Char.escaped (String.get s n) 
        else  Char.escaped (String.get s n)^(retourne_aux( n-1)s);;

let retourne (s:string):string = 
    if (String.length s)= 0 then "" 
    else retourne_aux ((String.length s)-1) s;;

(*Exercice 3 : question 12*);;
let rec est_premier_aux = fun n -> fun m->
  if (m = 0) then true
  else if (m = 1) then true
  else begin
      if ((mod) n m = 0) then false    
      else true && (est_premier_aux n (m-1)) end;;

(*question 13*);;
let rec est_premier_aux n m =
  if (m = 0) then true
  else if (m = 1) then true
  else begin
      if ((mod) n m = 0) then false    
      else true && (est_premier_aux n (m-1)) end;;

let est_premier x =
if x >=2 then est_premier_aux x (x-1) else false;;


(** Exercice 4 : Question 14 - Écrire la fonction récursive suivante
nb_occurence_aux: char -> string -> int -> int, telle que nb_occurence_aux: c s n renvoie 
le nombre de fois que le caractere c apparait dans les n premier caractere de s.
Vous aurez besoin de la fonction String.get : string -> int -> char qui renvoi le un caractere 
a une position donné dans une chaine de caractere.*);;

let rec nb_occurence_aux c s n =
  if n < 0 then  0
  else 
      let occ = (String.get s) n in
      if c = occ then 1 + nb_occurence_aux c s (n -1)
      else  nb_occurence_aux c s (n -1);;

(**Question 15*);;
(*Écrire la fonction suivante nb_occurence: char -> string -> int, telle que nb_occurence: c s renvoie 
le nombre de fois que le caractere c apparait dans s.
Vous aurez besoin de la fonction String.length : string -> int  qui renvoi la taille d'une chaine 
de caractere et la fonction de la question précédente.*);;

let rec nb_occurence_aux c s n =
  if n < 0 then 0
  else 
      let occ =(String.get s) n  in
      if c = occ then 1 + nb_occurence_aux c s (n-1)
      else nb_occurence_aux c s (n-1);;

    let nb_occurence c s =
   nb_occurence_aux c s ((String.length  s)-1);;


  nb_occurence_aux 'c' "coucou" 3;;