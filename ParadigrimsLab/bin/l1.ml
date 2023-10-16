(* zad 1 *)

let reverse4 = function (x, y, z, t) -> (t, z, y, x);;

reverse4 (1, 22, 3, 4);;

(* zad 2 *)

let rec sumProd = function (s, e) -> 
  (
    if s = e then (0, 1)
    else let (a, b) = sumProd(s+1, e) in (a+s, b*s)

  );;

sumProd (5, 8);;

(* zad 3 *)

let isPerfect = function (n) -> 
  (
    let rec sumN = function(x) -> 
      if x = 1 then 1
      else  if (float_of_int n /. float_of_int x) -. float_of_int(n / x) = 0.0 then (sumN(x - 1) + x)
      else sumN(x - 1)  
    in
    sumN (n - 1) = n
  );;

isPerfect 6;;
isPerfect 3;;
isPerfect 12;;

(* zad 4 *)

let rec insert = function (xs, el, pos) ->
  (
    if pos <= 0 then el::xs
    else if xs = [] then el::[]
    else let x, tail = List.hd xs, List.tl xs in x::insert(tail, el, pos -1)
  );;

  insert ([1; 3; 4], 2, 1);;

  insert ([1; 3; 4], 2, 10);;

  insert ([1; 3; 4], 2, 0);;

  insert ([1; 3; 4], 2, -1);;