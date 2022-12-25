(*question 13*);;
let min_arg2 f =  if ( f 1 ) <  (f 2) then 1 else 2;;

(*question 14*);;
let min_arg2 f =  if ( f 1 ) <  (f 2) then 1 else 2;;
let min_arg3 x = if (x 3) < (x 1) && (x 3) < (x 2) then 3 else min_arg2 x;;


(*question 15*);;
let min_arg2 f =  if ( f 1 ) <  (f 2) then 1 else 2;;
let min_arg3 x = if (x 3) < (x 1) && (x 3) < (x 2) then 3 else min_arg2 x;;
let min_arg4 y = if (y 4) < (y 1) && (y 4) < (y 2) && (y 4) < (y 3) then 4 else min_arg3 y;;


(*question 16*);;
let rec min_arg_n = fun n -> fun f -> 
    if n <= 1 then 1 
    else 
        let app = min_arg_n (n-1) f in 
        if  f n < f app then n
    else app;;
    
