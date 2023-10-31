(* ready code snippets to paste into REPL console*)

(*task 1*)
let rec flatten xss = 
  match xss with
  | [] -> []
  | head::tail -> List.append head (flatten tail)
;;

 flatten [[1; 2]; [3];[3;4]];; 
 flatten [[1; 2]; []; [3];[]];; 
 flatten [[]];; 
 flatten [];; 

(*task 2*)
let rec count x xs =
  match xs with
  | [] -> 0
  | head::tail when head=x -> 1 + count x tail
  | _::tail -> count x tail
;;

let rec count2 x xs =
  if xs = [] then 0
  else if List.hd xs = x then 1 + count x (List.tl xs)
  else count x (List.tl xs);;

count ('a', ['a'; 'b'; 'a'; 'c']);;
count 'e' ['a'; 'b'; 'a'; 'c'];;
count 2 [5; 4; 4];;
count 2 [];;

(*task 3*)
let rec replicate a num = 
  match num with
  | 0 -> []
  | n when n > 0 -> a::( replicate a (num - 1)) 
  | _ -> []
;;
let rec repl2 a num =
  if num <= 0 then []
  else a::repl2 a (num - 1);;

replicate 'x' 4;;
replicate 'x' 0;;
replicate 5 (-3);; 

(*task 4*)
let sqrList = function xs ->
  List.map(fun x -> x*x) xs
;;

let rec sqr2 xs=
  if xs = [] then []
  else List.hd xs * List.hd xs :: sqr2 (List.tl xs);;

sqrList [1; 2; 3; -4; -5];;
sqrList [];;
sqr2 [1; 2; 3; -4; -5];;

(*task 5*)
let palindrome xs =
  List.rev xs=xs
;;

palindrome [1; 2; 3; -4; -5];;
palindrome [1; 2; 3; 2; 1];;
palindrome [];;
palindrome [1; 1];;

(*task 6*)
let rec listLen xs =
  match xs with
  | [] -> 0
  | _::tail -> listLen tail + 1
;;

listLen [1; 2; 3; -4; -5];; 
listLen [];; 

(*task 7*)
let rec recursive c n =
  match n with
  | 1 -> 1
  | _ -> c * int_of_float(log(float_of_int n) /. log 2.0) + recursive c (n/2)
;;

recursive 1 8 ;;
recursive (-1) 4;; 
recursive 0 16;;

