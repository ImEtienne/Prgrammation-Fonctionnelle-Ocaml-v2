type directive = TurnLeft | TurnRight
      | StepForward of int
      | StepBackward of int;;
type path = directive list;;
let sample_path = (StepForward 1 :: StepForward 2:: TurnLeft ::
                     StepBackward 3 :: TurnLeft :: StepForward 1:: []);;

let inverse = fun x ->
  match x with
  StepForward x -> StepBackward x
  | StepBackward x -> StepForward x
  | TurnLeft  -> TurnRight 
  | TurnRight  -> TurnLeft ;;
  
let string_of_path_aux  = fun p -> 
  match p with
  | StepForward x -> "Avancer de " ^ (string_of_int x) ^ " pas" 
  | StepBackward x -> "Reculer de " ^ (string_of_int x) ^ " pas" 
  | TurnLeft -> "Tourner à gauche" 
  | TurnRight  -> "Tourner à droite";;

let rec string_of_path = fun a ->
  match a with 
  [] -> ""
  | n::[] -> string_of_path_aux n
  | n::q -> string_of_path_aux n ^ " ; " ^ string_of_path q;;
  
type orientation = 
  | North
  | South
  | East
  | West;;
  
type hunter = (int * int * orientation);;

let string_of_hunter chasseur = let (a,b, direction) = chasseur in
  match direction with
  | South -> "(" ^ (string_of_int a)^","^(string_of_int b)^","^ "sud"^ ")"
  | North -> "(" ^ (string_of_int a)^","^(string_of_int b)^","^ "nord"^ ")"
  | West -> "(" ^ (string_of_int a)^","^(string_of_int b)^","^ "ouest"^ ")"
  | East -> "(" ^ (string_of_int a)^","^(string_of_int b)^","^ "est"^ ")";;
  
let tourner_a_droite dir =
  match dir with
    | East -> South
    | South -> West
    | West -> North
    | North -> East;;

let tourner_a_gauche dir =
  match dir with
    | East -> North
    | North -> West
    | West -> South
    | South -> East;;

let move chasseur direction = let (x,y,dir) = chasseur in
  match direction with
    | StepForward a -> (match dir with
      | North -> (x,(y+a),dir)
      | East -> ((x+a),y,dir)
      | South -> (x,(y-a),dir)
      | West -> ((x-a),y,dir))
    | StepBackward a -> (match dir with
      | North -> (x,(y-a),dir)
      | East -> ((x-a),y,dir)
      | South -> (x,(y+a),dir)
      | West -> ((x+a),y,dir))
    | TurnLeft -> (x,y, tourner_a_gauche dir)
    | TurnRight -> (x,y, tourner_a_droite dir);;
    
let rec finally chasseur chemin = let (x,y,dir) = chasseur in
  match chemin with
    | [] -> (x,y,dir)
    | x::q -> finally (move chasseur x) q;;
    
type obstacles = (int * int) list;;

let move_with_obstacles obs chasseur direction = let (xm,ym,dirm) = move chasseur direction in
    match obs with
      | [] -> (xm,ym,dirm)
      | (obsx, obsy)::q -> (match direction with
        | StepForward a -> (match dirm with
          | North -> (xm,(ym+a),dirm)
          | East -> ((xm+a),ym,dirm)
          | South -> (xm,(ym-a),dirm)
          | West -> ((xm-a),ym,dirm))
        | StepBackward a -> (match dirm with
          | North -> (xm,(ym-a),dirm)
          | East -> ((xm-a),ym,dirm)
          | South -> (xm,(ym+a),dirm)
          | West -> ((xm+a),ym,dirm))
        | TurnLeft -> (xm,ym, tourner_a_gauche dirm)
        | TurnRight -> (xm,ym, tourner_a_droite dirm));;

        let rec move_with_obstacles_aux (x,y) (c,d) obs =
          match obs with
              | [] -> false
              | [(a,b)] -> if a >= x && a <= c && b >= y && b <= d then true else false
              | (a,b) :: r -> if a >= x && a <= c && b >= y && b <= d then true else move_with_obstacles_aux (x,y) (c,d) r;;
        
              let move_with_obstacles obs h d =
                let (x,y,o) = h in
                match d with
                | StepForward n -> if o = West then (
                        if move_with_obstacles_aux (x,y) (x-n,y) obs then h
                        else (x-n,y,o)
                    ) else if o = North then (
                        if move_with_obstacles_aux (x,y) (x,y+n) obs then h
                        else (x,y+n,o)
                    ) else if o = East then (
                        if move_with_obstacles_aux (x,y) (x+n,y) obs then h
                        else (x+n,y,o)
                    ) else if move_with_obstacles_aux (x,y) (x,y-n) obs then h else (x,y-n, o)
                | StepBackward n -> if o = West then (
                        if move_with_obstacles_aux (x,y) (x+n,y) obs then h
                        else (x+n,y,o)
                    ) else if o = North then (
                        if move_with_obstacles_aux (x,y) (x,y-n) obs then h
                        else (x,y-n,o)
                    ) else if o = East then (
                        if move_with_obstacles_aux (x,y) (x-n,y) obs then h
                        else (x-n,y,o)
                    ) else if move_with_obstacles_aux (x,y) (x,y+n) obs then h else (x,y+n, o)
                | d -> move h d;;

let rec finally_with_obstacles obs h p=
  List.fold_left (move_with_obstacles obs) h p;;