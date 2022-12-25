(**question 3*);;
let is_empty = fun (x,y)->
                if x > y then true
                else false;;

(**question 4*);;
let inclus = fun x -> fun (a,b)->
        if x >  a  && x < b then true
        else false;;

(**question 5*);;
let intersection = fun (a,b) -> fun (c,d)->
                (max a c, min b d);;