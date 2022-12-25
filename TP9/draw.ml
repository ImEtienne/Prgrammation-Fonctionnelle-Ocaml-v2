include DrawingType

let simple_triangle =
  let pi23 = 2.0 *. pi /. 3.0 in
  Forward 25.0 :: Turn pi23 ::
  Forward 25.0 :: Turn pi23 ::
  Forward 25.0 :: []

let simple_square =
  Forward 25.0 :: Turn pi2 ::
  Forward 25.0 :: Turn pi2 ::
  Forward 25.0 :: Turn pi2 ::
  Forward 25.0 :: []

let pentagone =
  let pi2sur5 = (pi *. 2.) /. 5. in
  Forward 25.0 :: Turn pi2sur5 ::
  Forward 25.0 :: Turn pi2sur5 ::
  Forward 25.0 :: Turn pi2sur5 ::
  Forward 25.0 :: Turn pi2sur5 ::
  Forward 25.0 :: []

let rec polygone_aux n taille angle =
  if n = 0 then []
  else Forward 25.0 :: Turn angle :: polygone_aux (n-1) taille angle;;

let polygone n =
  polygone_aux n 25.0 ((pi *. 2.) /. float_of_int n);;

let rec square_spiral n d =
  if n = 0 then []
  else Forward d :: Turn pi2 :: square_spiral (n-1) (d*.0.8);;

let rec spiral_log_ret_aux n d rho l m =
  if n = m then l
  else Turn (-.rho) :: Forward (-.(d/.0.8)) :: spiral_log_ret_aux (n+1) (d/.0.8) rho l m;;

let rec spiral_log_aux n d rho l m=
  if n = 0 then spiral_log_ret_aux n d rho l m
  else Forward d :: Turn rho :: spiral_log_aux (n-1) (d*.0.8) rho l m;; 

let spiral_log n d rho l =
  spiral_log_aux n d rho l n;;

let draw = fun n ->
  spiral_log 50 10. 0.5 ( spiral_log 50 10. 1. [] );;

let rec tree n d rho l =
  if n=0 then l
  else 
    let l2 = Turn(_,rho) ::  Forward (_,d):: l in
    let l3 = tree(n-1) (0.8*.d) rho l2 in 
    let l4 = Forward d:: Turn rho :: l3 in
    let l5 = Turn (rho) :: Forward (_,d) :: l4 in
    let l6 = tree (n-1) (0.8*.d) rho l2 in
    Forward d ::Turn (_,rho) ::l6;;

let rec spl = fun n d rho l ->   if n=0 then l   else apply_draw (Forward d)         
      (apply_draw (Turn rho) (spl (n-1) (0.8*.d) rho))         l


let draw n = 
  let s1 = tree n 10.0 (0.1) [] in s1

