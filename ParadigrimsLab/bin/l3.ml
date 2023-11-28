(* let (>>)f n  = fun x ->
  let rec partialRes n res list = 
    match n with
    | nn when nn <= 0 -> list
    | _ -> partialRes (n-1) (f res) (res::list)
  in
  partialRes n x [];; *)


  let (>>)f n  = fun x ->
    let rec partialRes n res list = 
      match n with
      | nn when nn <= 0 -> list
      | _ -> partialRes (n-1) (f (res - 1) )(res::list)
    in
    partialRes n (x) [];;
  

  let testFun x = x*x;;

  testFun >> 5;;

 (testFun >> 0 ) 1;;

 (testFun >> 1 ) 2;;

 (testFun >> 2 ) 2;;

 (testFun >> 6 ) 2;;

 (testFun >> -3 ) (-2);;

 (testFun >> 3) (-2);;
