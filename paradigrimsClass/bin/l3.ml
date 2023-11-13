(* zad 2 *)

let curry3 f x y z = f (x, y, z)

let curry3l = function f -> function x -> function y -> function z -> f(x, y, z)

let uncurry3 f (x, y, z) = f x y z

let uncurry3l f = function (x, y, z) -> f x y z 


(* zad 3 *)

let sumProd xs = List.fold_left (fun (sum, prod) (el) -> (sum + el), (prod * el)) (0, 1) xs;;

sumProd [1; 2; 3; 4; 5; 6];;
sumProd [];;
sumProd [2];;
sumProd [-2; -1];;
sumProd [-2; -1; 0; 1];;
sumProd [-2; -1; 8; 9; 10];;



  (* zad 5 *)
let rec insertionsort (compare) (list) =
  let rec insert x = function
    | [] -> [x] 
    | y::ys -> if compare x y <= 0 then x::y::ys else y::insert x ys
  in
  match list with
    | [] -> []
    | x::xs -> insert x (insertionsort compare xs)
  
;;

let rec mergesort (compare) (list) = 
  let rec merge xs ys =
    match (xs, ys) with
    | [], _ -> ys
    | _, [] -> xs
    | x::xs, y::ys -> if compare x y <= 0 
      then x::merge xs (y::ys) 
      else y::merge (x::xs) ys
  in
  let rec split = function
    | [] -> [], []
    | [x] -> [x], []
    | x::y::zs -> let (xs, ys) = split zs 
  in 
  x::xs, y::ys
  in
  match list with
    | [] -> []
    | [x] -> [x]
    | _ -> let (xs, ys) = split list 
  in merge (mergesort compare xs) (mergesort compare ys)
;;

let compare_pairs (a1, _) (b1, _) = compare a1 b1;;
insertionsort compare_pairs [(11, 2); (2, 3); (31, 4); (2, 5); (5, 6)];;
insertionsort compare_pairs [(8, 1); (-2, 2); (8, 3); (3, 4); (-2, 5)];;



insertionsort compare [1; 21; 31; 4; 15; 6];;
insertionsort compare [];;
insertionsort compare [2];;
insertionsort compare [-2; -1];;
insertionsort compare [-2; -1; 0; 1];;
insertionsort compare [-2; -13; 8; 10; 1];;
insertionsort compare [-2; -13; 8; 10; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0];;
insertionsort compare [-2; -13; 8; 10; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; -1];;


mergesort compare [1; 21; 31; 4; 15; 6];;
mergesort compare [];;
mergesort compare [2];;
mergesort compare [-2; -1];;
mergesort compare [-2; -1; 0; 1];;
mergesort compare [-2; -13; 8; 10; 1];;
mergesort compare [-2; -13; 8; 10; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0];;
mergesort compare [-2; -13; 8; 10; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; -1];;


