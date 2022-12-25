(**Question 1*)
type expr = 
  | Number of int
  | Variable of string
  | Minus of expr
  | Add of expr * expr
  | Multiply of expr * expr

(*Question 2*)

let ex = Multiply ( Add (Number (3),  Minus (Variable ("y"))), Number (5));;

(*question 3*)

let rec string_of_expr = fun expr -> 
  match expr with
  | Number n -> (string_of_int n)
  | Variable n -> n
  | Minus n -> "( -"^string_of_expr (n) ^")"
  | Multiply (x1, x2)  -> "("^string_of_expr(x1)^" * "^string_of_expr(x2)^")"
  | Add (y1,y2) -> "("^string_of_expr(y1)^" + "^string_of_expr(y2)^")";;

let rec eval_simple = fun expr -> 
  match expr with
  | Number n -> n
  | Variable n -> raise  (Invalid_argument "Impossible a evaluer !")
  | Minus n -> eval_simple(n) * -1
  | Multiply (x1, x2)  -> eval_simple(x1) * eval_simple(x2)
  | Add (y1,y2) -> eval_simple(y1) + eval_simple(y2);;

(*let rec eval_complex expr =
  match expr with
  | Number n -> Number n
  | Variable n -> Variable n
  | Minus n -> 
    (match (eval_complex n) with 
       (Number n1) -> Number (n1 * -1)
     | _ -> Minus (eval_complex n))
  | Add (y1,y2) -> 
    (match ( eval_complex y1, eval_complex y2) with
       (Number a, Number b) -> Number (a + b)
     | _ -> Add ( eval_complex y1, eval_complex y2))

  | Multiply (x1, x2)  -> 
    (match (eval_complex x1, eval_complex x2) with
       (Number a1, Number b1) -> Number (a1 * b1)
     | _-> Multiply(eval_complex x1, eval_complex x2));;*)

let rec eval_complex x =
  (match x with
     Number n -> Number n
   |Variable v -> Variable v
   |Minus e -> (match e with
         Number n -> Number (-n)
       |Minus (Number n)-> Number n
       |Minus (Minus (Number n)) -> Number (-n)
       |_-> Minus (eval_complex e))
   |Add (e1,e2) -> (match (e1,e2) with
         (Number n1, Number n2) -> Number (n1+n2)
       |(Number n1,Variable v) -> Add(Number n1, Variable v)
       |(Variable v1,Variable v2) -> Add(Variable v1, Variable v2)
       | _ -> Add((eval_complex e1), (eval_complex e2)))
   |Multiply(e1,e2) -> (match (e1,e2) with
         (Number n1,Number n2)-> Number(n1*n2)
       |(Number n1, Variable v)-> Multiply(Number n1, Variable v)
       |(Variable v, Number n2)-> Multiply(Variable v, Number n2)
       |(Variable v1, Variable v2)-> Multiply(Variable v1, Variable v2)
       | _-> Multiply(eval_complex e1, eval_complex e2)));;

let rec find_space s i =
  if i >= String.length s then i
  else if int_of_char (String.get s i)  = 32 then i
  else find_space s (i+1);;

let rec tokenize_aux = fun s -> fun i ->
  if i >= String.length s then []
  else if ((find_space s i) + 1) >= String.length s then (String.sub s i ((find_space s i) - i)) :: []
  else (String.sub s i ((find_space s i) - i)) :: tokenize_aux s ((find_space s i) + 1);;

let tokenize = fun s -> tokenize_aux s 0;;


let ex2 = Add (Add (Minus (Number 7), (Number 3)), Number 2)
exception Ill_formed_expr
let make_polish_aux1 expr1 s = match expr1,s with
  |e1::q,"-" -> Minus(e1)::q
  |l,"" -> Variable ""::l
  |l," " -> Variable " "::l
  |l,x when String.length x > 1 -> Variable x::l
  |l,x when Char.code (String.get x 0) < 48  -> Variable x::l
  |l,x when Char.code (String.get x 0) > 57 -> Variable x::l
  |[],"-" -> raise  (Invalid_argument "Impossible a evaluer !")
  |e1::e2::q,"+" -> Add(e1,e2)::q
  |e1::e2::q,"*" -> Multiply(e1,e2)::q
  |l,x -> Number (Char.code (String.get x 0))::l

let rec make_polish_aux2 expr s = match s with
  [] -> expr
  |e::q -> make_polish_aux2 (make_polish_aux1 expr e) q
let make_polish l = make_polish_aux2[] l
let expr_of_string = fun str -> match (make_polish (tokenize str)) with
  |[] -> raise(Ill_formed_expr)
  |t1::t2::q -> raise(Ill_formed_expr)
  |t::q -> t
  let compute s = string_of_expr (eval_complex (expr_of_string s))
