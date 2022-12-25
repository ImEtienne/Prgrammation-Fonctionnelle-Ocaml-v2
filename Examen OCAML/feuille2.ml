
5::[];;
let mfun x = (true, x,x );;

let rec fq_4 n x = 
  if n = x then [n]
  else fq_4(n-1) x;;

fq_4 5 2;;

let rec fq_7  x a = 
  if fq_7 (x-1) = 2 then Some x
  else None;;

let fq_9 a b = if  then Some a
  else None

type etudiant = {
  Nom: String;
  Annee: int;
}

type personnel = {
  Nom : String;
  Numero: int;
}

let fq_12 (a,b) = a b ()




let rec pow2 x n = 
  if n = 0 then x
  else 2.0 *. pow2 x (n - 1);;

pow2 2.5 3;;
