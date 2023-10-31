(* :: czyli konstruktor listy wiaze w lewo czyli el::(el2::tail) *)

(* lista 5-cio elementowych krotek *)
['H', 'e', 'l', 'l', 'o'];; 
(* lista charow *)
['H'; 'e'; 'l'; 'l'; 'o'];;

(* List.hd uzyta na pustej liscie ([]) wyrzuca blad *)
(* List.tl uzyte na pustej liscie [] wyrzuca blad *)

let rec chop (xs, n) = 
  if xs = [] || n <= 0 then []
  else List.hd xs::(chop (List.tl xs, n - 1 ));;

chop([1; 2; 3; 4; 5], -5);;
chop([1; 2; 3; 4; 5], 0);;
chop([1; 2; 3; 4; 5], 2);;
chop([1; 2; 3; 4; 5], 4);;
chop([1; 2; 3; 4; 5], 5);;

(* Transparentnosc referencyjna: poniewaz nigdy nie modyfikujemy naszych zmiennych oraz *)
(* nasze funckje sa czyste:  *)
(* kazde wyrazenie, ktore mamy w kodzie moze zostaac zastapione przez jego wartosc *)