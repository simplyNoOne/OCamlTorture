(* ready code snippets to paste into REPL console*)

(*task 1*)
let rec flatten xss = 
  match xss with
  | [] -> []
  | head::tail -> List.append head (flatten tail)
;;

(* flatten [[1; 2]; [3];[3;4]];; *)

(*task 2*)
let rec count xs x=
  match xs with
  | [] -> 0
  | head::tail when head=x -> count tail x + 1
  | _::tail -> count tail x
;;

(* count ['a'; 'b'; 'a'; 'c'] ;;*)


(*task 3*)
let rec replicate a num = 
  match num with
  | 0 -> []
  | n when n > 0 -> a::( replicate a (num - 1)) 
  | _ -> []
;;

(* replicate 'x' 4;; *)


(*task 4*)
let sqrList xs = 
  List.map(fun x -> x*x) xs
;;

(* sqrList [1; 2; 3; -4; -5];; *)

(*task 1*)
let palindrome xs =
  List.rev xs=xs
;;

(* palindrome [1; 2; 3; -4; -5];; *)
(* palindrome [1; 2; 3; 2; 1];; *)

(*task 6*)
let rec listLen xs =
  match xs with
  | [] -> 0
  | _::tail -> listLen tail + 1
;;

(* listLen [1; 2; 3; -4; -5];; *)

(*task 7*)
let rec recursive c n =
  match n with
  | 1 -> 1
  | _ -> c * int_of_float (log(float_of_int n) /. log 2.0) + recursive c (n/2)
;;

(* recursive 1 8 *)