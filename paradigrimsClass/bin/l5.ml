
type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;
let rec lfrom k = LCons (k, lazy (lfrom (k+1)));;

let rec toLazyList = function
[] -> LNil
| x :: xs -> LCons(x, lazy (toLazyList xs))
;;

let rec ltake = function
(0, _) -> []
| (_, LNil) -> []
| (n, LCons(x, lazy xs)) -> x :: ltake(n-1, xs)
;;

let rec lzip (lxs, lys) =
match (lxs, lys) with
(LCons(h1, lazy t1), LCons(h2, lazy t2)) -> LCons((h1, h2), lazy (lzip (t1, t2)))
| _ -> LNil
;;
(* val lzip : 'a llist * 'b llist -> ('a * 'b) llist = <fun> *)

(* zad 1 *)

let rec lrepeat k lxs =
  let rec repeat_element n xs =
    match xs with
    | LNil -> LNil
    | LCons (h, lazy t) -> 
      if n <= 0 then (repeat_element k t)
      else LCons (h, lazy (repeat_element (n - 1) xs))
    
  in repeat_element k lxs
;;

ltake (15, lrepeat 3 (toLazyList [1;2;3;4;5]));;
(* - : int list = [1; 1; 1; 2; 2; 2; 3; 3; 3; 4; 4; 4; 5; 5; 5] *)

ltake (15, lrepeat 0 (toLazyList [1;2;3;4;5]));;
(* - : int list = [] *)

ltake (15, lrepeat (-1) (toLazyList [1;2;3;4;5]));;
(* zad 2 *)

let lfib = 
  let rec fib a b = LCons(a, lazy (fib b (a + b)))
in fib 0 1;;

ltake (15, lfib);;

ltake (10, lfib);;

ltake (20, lfib);;



(* zad 3 a *)
type 'a lBT = LEmpty | LNode of 'a * (unit ->'a lBT) * (unit -> 'a lBT);;

let lBreadth lbt =
  let rec helper queue =
    match queue with
    | [] -> LNil
    | LEmpty :: rest -> helper rest
    | LNode(n, l, r) :: rest ->
      LCons(n, lazy (helper (rest @ [l();r()])))
  in
  match lbt with
  | LEmpty -> LNil
  | LNode(n, l, r)-> LCons( n, lazy (helper [l();r()]))
;;

let lt = LNode(1, 
  (fun () -> LNode(2, (fun () -> LNode(4, (fun () -> LEmpty), (fun () -> LEmpty))), (fun () -> LNode(5, (fun () -> LEmpty), (fun () -> LEmpty))))),
  (fun () -> LNode(3, (fun () -> LNode(6, (fun () -> LEmpty), (fun () -> LEmpty))), (fun () -> LEmpty)))
);;

(* will take the entire lazy List *)
ltake (-2, lBreadth lt);; 

ltake (3, lBreadth lt);; 

ltake (5, lBreadth lt);; 

ltake (9, lBreadth lt);; 
(* - : int list = [1; 2; 3; 4; 5; 6] *)

(* zad 3 b *)

let rec lTree n = 
  LNode(n, (fun ()-> (lTree( 2*n ))), (fun() -> (lTree(2*n + 1))))
;; 

ltake(20, lBreadth (lTree 1));;

ltake(100, lBreadth (lTree 2));;

ltake(5, lBreadth (lTree 3));;

ltake(30, lBreadth (lTree 1));;