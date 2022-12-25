let nun = fun x1-> fun y1-> fun x2-> fun y2-> 
max (abs_float(x2 -. x1)) (abs_float(y2 -. y1));;

let rec perm = fun n -> fun k -> 
        if n>k then n * perm (n-1) k 
        else if n=k then 1 
        else 0 ;;
  
perm 5 50;;
