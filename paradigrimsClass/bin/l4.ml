
type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

type 'a graph = Graph of ('a -> 'a list);;

let tt = Node(1,Node(2,Node(4,Empty,Empty),Empty),Node(3,Node(5,Empty,Node(6,Empty,Empty)),Empty));;

let g = Graph(function 0 -> [3] | 1 -> [0;2;4] | 2 -> [1] | 3 -> [] | 4 -> [0;2] | n -> failwith ("Graph g: node "^string_of_int n^" doesn't exist"));;

(* zad 2 *)

let makeLeaf x = Node(x, Empty, Empty);;

(* zad 3 *)

let breadthBT tree = 
  let rec traverse queue = 
    match queue with
    [] -> []
    | Empty::t -> traverse t
    | Node(n, r, l)::t -> n::traverse(t@[r;l])
  in traverse [tree]
;;

breadthBT tt;;


(* zad 4 *)

let internalPath tree =
  let rec traverse tree depth =
    match tree with
    Empty -> 0
    | Node(_, r, l) -> depth + traverse r (depth + 1) + traverse l (depth + 1)
  in traverse tree 0
;;

let externalPath tree =
  let rec traverse tree depth =
    match tree with
    Empty -> depth
    | Node(_, r, l) -> traverse r (depth + 1) + traverse l (depth + 1)
  in traverse tree 0
;;

internalPath tt;;
externalPath tt;;

(* zad 5 *)

let depthSearch (Graph graph) node = 
  let rec traverse visited queue =
    match queue with
    [] -> visited
    | h::t -> if List.mem h visited then traverse visited t else traverse (h::visited) (graph h @ t)
  in  List.rev (traverse [] [node])
;;

depthSearch g 1;;

depthSearch g 4;;

depthSearch g 3;;

depthSearch g 5;;