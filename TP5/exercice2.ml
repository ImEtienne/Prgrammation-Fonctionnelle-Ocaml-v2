(**question 11*);;
let  is_letter = fun c ->
          (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
         
let is_capital = fun c ->
         c >= 'A' && c <= 'Z'

let to_lower_case = fun c ->
        if (is_capital c) then (char_of_int ( ( int_of_char c) + 32))
        else c;;
let rec check_letter_aux = fun s -> fun i ->  
        if (i >= (String.length s)) then true 
        else if (is_letter (String.get s i)) then 
          (check_letter_aux s (i+1))
        else false;;
let check_letter = fun s -> check_letter_aux s 0;;

     
(**question 12*);;
let is_capital = fun c -> c >= 'A' && c <= 'Z';;
         
let rec find_capital = fun s -> fun i -> if (i >= (String.length s)) then i 
                else if (not (is_capital(String.get s i))) then find_capital s (i+1) 
                else i;;
                

(**question 13*);;
let  is_letter = fun c ->
          (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
         
let is_capital = fun c ->
         c >= 'A' && c <= 'Z'

let to_lower_case = fun c ->
        if (is_capital c) then (char_of_int ( ( int_of_char c) + 32))
        else c;;
let rec check_letter_aux = fun s -> fun i ->  
        if (i >= (String.length s)) then true 
        else if (is_letter (String.get s i)) then 
          (check_letter_aux s (i+1))
        else false;;
let check_letter = fun s -> check_letter_aux s 0;;

let rec caml_to_split_aux = fun s -> fun i->
    if i >= String.length s then ""
    else if is_capital (String.get s i) && i = 0 then Char.escaped (to_lower_case (String.get s i)) ^ caml_to_split_aux s (i+1)
    else if is_capital (String.get s i) then " " ^ Char.escaped (to_lower_case (String.get s i)) ^ caml_to_split_aux s (i+1)
    else Char.escaped (String.get s i) ^ caml_to_split_aux s (i+1)

let caml_to_split = fun s ->
    if check_letter s then caml_to_split_aux s 0
    else "";;

(**question 14*);;
let  is_letter = fun c ->
          (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
         
let is_capital = fun c ->
         c >= 'A' && c <= 'Z'

let to_lower_case = fun c ->
        if (is_capital c) then (char_of_int ( ( int_of_char c) + 32))
        else c;;
let rec check_letter_aux = fun s -> fun i ->  
        if (i >= (String.length s)) then true 
        else if (is_letter (String.get s i)) then 
          (check_letter_aux s (i+1))
        else false;;
let check_letter = fun s -> check_letter_aux s 0;;

let rec caml_to_split_list_aux = fun s -> fun i -> fun l ->
if i >= String.length s then l::[]
else if is_capital (String.get s i) && i = 0 then caml_to_split_list_aux s (i+1) (Char.escaped (to_lower_case (String.get s i)))
 else if is_capital (String.get s i) then l::caml_to_split_list_aux s (i+1) (Char.escaped (to_lower_case (String.get s i)))
 else caml_to_split_list_aux s (i+1) (l ^ Char.escaped (String.get s i));;

let caml_to_split_list = fun s ->
if s = "" || check_letter s = false then []
 else caml_to_split_list_aux s 0 "" ;;