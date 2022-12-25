(**question 8*);;
let applique2 = fun x -> fun f ->
            f ( f x);;

(**question 9*);;
let applique2_2 = fun f x ->
            f ( f x);;


(**question 10*);;
let rec applique_n = fun n -> fun f -> fun x ->
        if n = 0 then x
        else f (applique_n (n -1) f x);;