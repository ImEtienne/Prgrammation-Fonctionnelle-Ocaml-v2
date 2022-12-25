(**question 11*);;
let frac_mul = fun(a,b) -> fun(c,d) ->
    (a*c , b*d);;

(**question 12*);;
let frac_add = fun (a,b) -> fun (c,d)->
    (a*d + c*b), (b*d);;

(**question 13*);;
let rec pgcd = fun (a, b) ->
    if b = 0 then a
             else pgcd (b,a mod b);;


let frac_irr = fun (a, b) -> if (a*b<0 ) then
           (-1 * (abs (a / pgcd (a,b))), abs( b / pgcd (a,b )))
           else (abs (a / pgcd (a,b)), abs( b / pgcd (a,b )));;

(**question 14*);;

let rec pgcd = fun (a, b) ->
    if b = 0 then a
             else pgcd (b,a mod b);;


let frac_irr = fun (a, b) -> if (a*b <0 ) then
           (-1 * (abs (a / pgcd (a,b))), abs( b / pgcd (a,b )))
           else (abs (a / pgcd (a,b)), abs( b / pgcd (a,b )));;
           
let frac_eq = fun (a,b) -> fun (c,d) -> if frac_irr (a,b)= frac_irr (c,d) then true else false;;
            

    