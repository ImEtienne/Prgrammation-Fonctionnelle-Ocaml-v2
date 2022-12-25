(**question 6*);;
let rec fibonacci_1 = fun x ->
        if x <= 1 then 1
        else fibonacci_1 (x -1) + fibonacci_1(x - 2);;

(**question 7*);;
let rec fibonacci_2 = fun n ->
        if n <= 1 then (n, 1)
        else let (f1,f2) = fibonacci_2 (n-1) in 
            (f2, f1 + f2 );;
            