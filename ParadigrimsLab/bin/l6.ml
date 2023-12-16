
let declaracci m n =
  let rec fib a b cnt=
    if cnt = 0 then []
    else a ::(fib b (a + b) (cnt - 1))
  in 
  let rec fibVal c d k =
    if k = 0 then 0
    else if k = 1 then c
    else fibVal d (c + d) (k - 1)
  in
  let start = 1 + (m - 1) * 2 in 
  fib (fibVal 1 1 start)(fibVal 1 1 (start + 1)) n
;; 

declaracci 1 5;;
declaracci 2 8;;
declaracci 3 13;; 
declaracci 4 3;; 
declaracci 5 6;; 


let imperiacci m n =
  let start = 1 + (m - 1) * 2 in
  let id = ref 1 in
  let a = ref 1 and b = ref 1 in
  while !id < start do 
    let aux = !a in
    a := !b;
    b :=  (aux + !b);
    id := (!id + 1);
  done;
  let skip = Array.make n 0 in 
  id := 0;
  while !id < n do 
    skip.(!id) <- !a;
    a := !b;
    b := (!b + skip.(!id));
    id :=  (!id + 1);
  done;
  skip  
;;


imperiacci 1 5;;
imperiacci 2 8;;
imperiacci 3 13;;
imperiacci 4 3;;
imperiacci 5 6;;

