


let argMax = function xs ->
  if xs = [] then []
  else 
    let rec helper = function list, i ->
      if list =[] then (List.hd xs, [])
      else
        let (a,b) = helper (List.tl list, i+1) in
      
        if List.hd list = a then a, i::b
        else if List.hd list > a then List.hd list, i::[]
        else (a, b)
    in
    let (_, b) = helper (xs, 0) in b
;;


  argMax [1; 2; 7; 5; 7; 2; 7; 1];;

  argMax [1; 2; -7; 5; -7; 2; -7; 1];;

  argMax [];;

  argMax [1; -7; 1];;

  argMax [1; 1; 1];;

  argMax [9; -7; 1];;

  argMax [1; -7; 1; 9];;

  argMax [-1; -7; -1];;
