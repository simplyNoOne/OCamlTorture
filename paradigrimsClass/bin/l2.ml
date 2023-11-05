
(* zad 2 *)
let rec fib = function n ->
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib (n-2) + fib (n-1)
;;

let fibTail = function n ->
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> (
    let rec aux = function (x, acc1, acc2) ->
    match x with
    | nn when nn = n -> acc1 + acc2
    | _ -> aux (x+1, acc2, acc1+acc2)
  in
  aux (2, 0, 1)
  )
;;

fib 5;;
fibTail 5;;

fib 20;;
fibTail 20;;

(* fib 42;; *)
fibTail 42;;
(* jakies 100 razy szybsza *)


(* zad 3 *)

let root3 = function a ->
  let rec aux = function acc ->
    let next = acc +. (a /. (acc *. acc) -. acc)/. 3.0 in
    match abs_float(next *. next *. next -. a) <= 10.0 ** (-15.0) *. abs_float(a) with
    | true -> next
    | false -> aux next
  in
  if a > 1.0 then aux (a /. 3.0)
  else if a = 0.0 then 0.0
  else aux a
;;


root3 8.0;;
root3 (-8.0);;
root3 0.0;;
root3 1000000.0;;
root3 8940560.0;;

(* zad 4 *)

let _::_::x::_::_::[] = [-2; -1; 0; 1; 2];;

let _::(x, _)::[] = [(1,2 ); (0, 1)];;

(* zad 5 *)
let rec initSegment= function list1, list2 ->
  match list1, list2 with
  | [], _ -> true
  | _, [] -> false
  | h1::t1, h2::t2 -> h1 = h2 && initSegment (t1, t2)
;;

initSegment ([1; 2], [1; 2; 3]);;
initSegment ([1; 2], [1; 2]);;
initSegment ([1; 2], [1; 3]);;
initSegment ([1; 2], [1; 1; 3]);;
initSegment ([1; 2], [1]);;
initSegment ([], [1; 2; 3]);;
initSegment ([1; 2], []);;
initSegment ([], []);;

(* zad 6 *)

let rec replaceNth = function xs, n, x ->
  match xs, n with
  | list, a when a < 0 -> list 
  | [], _ -> []
  | _::t, 0 -> x::t
  | h::t, _ -> h::replaceNth(t, n-1, x)
;;

replaceNth (['a'; 'b'; 'c'; 'd'], 2, 'x');;
replaceNth (['a'; 'b'; 'c'; 'd'], 0, 'x');;
replaceNth (['a'; 'b'; 'c'; 'd'], 3, 'x');;
replaceNth (['a'; 'b'; 'c'; 'd'], 4, 'x');;
replaceNth (['a'; 'b'; 'c'; 'd'], -1, 'x');;
replaceNth ([], 2, 'x');;
replaceNth ([], 0, 'x');;