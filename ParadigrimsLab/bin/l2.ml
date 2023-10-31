(* zad 1 *)

let rec cutAndMend (a,  b,  xs) = 
  match (a, b, xs, a >= 0) with
  | _, _, _, false -> []
  | _, _, [], _ -> []
  | 0, 0, h::_, _ -> h::[]
  | 0, _, h::t, _ -> h::cutAndMend  (0,  b-1,  t )
  | _, _, _::t, _ ->  cutAndMend (a-1 , b-1,  t);;



  cutAndMend (2, 4, [1; 2; 3; 4; 5; 6; 7] );;
  cutAndMend (0, 4, [1; 2; 3; 4; 5; 6; 7] );;
  cutAndMend (2, 6, [1; 2; 3; 4; 5; 6; 7] );;
  cutAndMend (2, 9, [1; 2; 3; 4; 5; 6; 7] );;
  cutAndMend (-3, 4, [1; 2; 3; 4; 5; 6; 7] );;
  cutAndMend (-6, -4, [1; 2; 3; 4; 5; 6; 7] );;


  (* zad 2 *)

  let rec split2Rec xs =
    match xs with 
    | a::b::t -> let (t1, t2) = split2Rec t in a::t1, b::t2
    | _ -> [], []
  ;;

  let split2Tail xs =
    let rec aux (list, acc1, acc2) =
      match list with
      | a::b::t -> aux (t, a::acc1, b::acc2)
      | _ -> acc1,acc2
    in
    aux(xs, [], [])
  ;;

split2Rec [1; 2; 3; 4; 5; 6];;
split2Rec [1; 2; 3; 4; 5];;
split2Rec [1];;
split2Rec [];;

split2Tail [1; 2; 3; 4; 5; 6];;
split2Tail [1; 2; 3; 4; 5];;
split2Tail [1];;
split2Tail [];;