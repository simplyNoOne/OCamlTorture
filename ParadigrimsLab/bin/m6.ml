let findComplex n = 
  let isComplex = Array.make (n - 2) 0 in
  let num = ref 0 in
  let id = ref 2 in 
  while ((!id * !id) <= n) do 
    let j = ref 2 in 
    while ( !j * !id <= n ) do
      if (isComplex.(!j * !id - 3) = 0) then 
        (
          num := !num + 1;
          isComplex.(!j * !id - 3) <- 1;
        );
      j := !j + 1;
    done;
    id := !id + 1;
  done;
  let complex = Array.make !num 0 in
  let iter = ref 0 in
  for i = 0 to (n-3) do
    if (isComplex.(i) = 1) then
      (
      complex.(!iter) <- i + 3;
      iter := !iter + 1;
      )
  done;
complex
;;


findComplex 100;;