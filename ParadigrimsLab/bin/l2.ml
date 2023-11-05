(* zad 1 *)

let rec cutAndMend (a,  b,  xs) = 
  match (a, b, xs, a >= 0, b>=0) with
  | _, _, h::t, false, true -> h::cutAndMend (0, b-1, t)
  | _, _, _, _, false -> []
  | _, _, [], _, _ -> []
  | 0, 0, h::_, _, _ -> h::[]
  | 0, _, h::t, _, _ -> h::cutAndMend  (0,  b-1,  t )
  | _, _, _::t, _, _ ->  cutAndMend (a-1 , b-1,  t);;



  cutAndMend (2, 4, [1; 2; 3; 4; 5; 6; 7] );;
  cutAndMend (0, 4, [1; 2; 3; 4; 5; 6; 7] );;
  cutAndMend (2, 6, [1; 2; 3; 4; 5; 6; 7] );;
  cutAndMend (2, 9, [1; 2; 3; 4; 5; 6; 7] );;
  cutAndMend (-3, 4, [1; 2; 3; 4; 5; 6; 7] );;
  cutAndMend (-6, -4, [1; 2; 3; 4; 5; 6; 7] );;

  cutAndMend (-1, 9, ['f'; 'f'; '3'; 'a'] );;
  cutAndMend (0, 2, [ 'f'] );;


  let cutAndMend15 xs = cutAndMend (1, 5, xs);;
  let cutAndMend23 xs = cutAndMend (2, 3, xs);;
  let cutAndMend33 xs = cutAndMend (3, 3, xs);;


  cutAndMend15 ([1; 2; 3; 4; 5; 6; 7] );;
  cutAndMend23 ([1; 2; 3; 4; 5; 6; 7] );;
  cutAndMend33 ( [1; 2; 3; 4; 5; 6; 7] );;
  cutAndMend33 ( ['d'] );;
  cutAndMend15 ( [1; 2; 3 ] );;


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
