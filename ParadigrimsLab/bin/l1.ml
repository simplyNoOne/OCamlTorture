(* zad 1 *)

let reverse4 = function (x, y, z, t) -> (t, z, y, x);;

reverse4 (1, 22, 3, 4);;
reverse4 (1, 'g', 3, 4.4);;
reverse4 (1, [], 3, [4; 3]);;

(* zad 2 *)

let rec sumProd = function (s, e) -> 
  (
    if s = e then (0, 1)
    else let (a, b) = sumProd(s+1, e) in (a+s, b*s)

  );;

sumProd (5, 8);;
sumProd (7, 8);;
sumProd (-8, -5);;
sumProd (-7, -4);;
sumProd (-8, -7);;
sumProd (-2, 4);;

(* zad 3 *)

let isPerfect = function n -> 
  (
    if n = 0 then false
    else
      (* sumDivX returns the sum of all divisors of n that are NOT GREATER than x *)
      let rec sumDivX = function x -> 
        if x = 1 then 1
        (* I'M STUPID I COULDN'T FIND MODULO else  if (float_of_int n /. float_of_int x) -. float_of_int(n / x) = 0.0 then (sumDivX(x - 1) + x) *)
        else if n mod x = 0 then (sumDivX(x - 1) + x)
        else sumDivX(x - 1)  
      in
      sumDivX (n - 1) = n
  );;

isPerfect 6;;
isPerfect 3;;
isPerfect 12;;
isPerfect 0;;
isPerfect 28;;

(* zad 4 *)

let rec insert = function xs, el, pos ->
  (
    if pos <= 0 then el::xs
    else if xs = [] then el::[]
    else let x, tail = List.hd xs, List.tl xs in x::insert(tail, el, pos - 1)
  );;

  insert ([1; 3; 4], 2, 1);;
  insert ([1; 3; 4], 2, 10);;
  insert ([1; 3; 4], 2, 0);;
  insert ([1; 3; 4], 2, -1);;
  insert ([], 'f', 3);;
  insert ([], [3; 43; 54], -1);;
  insert ([[];[]], [], 4);;
  insert ([[43; 9; 9]], [3; 43; 54], 0);;
  insert ([[]], [3; 43; 54], 41);;
  insert ([(43, 9)], (3, 43), -1);;
