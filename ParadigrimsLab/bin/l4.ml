 
(* 1 *)

type 'a tree3 = TEmpty | TNode of 'a * ('a)tree3 * ('a)tree3 * ('a)tree3;;

let rec mapTree3 f tree =
  match tree with
  | TEmpty -> TEmpty
  | TNode (x, b1, b2, b3) -> TNode (f x, mapTree3 f b1, mapTree3 f b2, mapTree3 f b3)
;; 

let t1 = TNode (1, TNode(11, TNode(111, TEmpty, TEmpty, TEmpty), TNode(112, TEmpty, TEmpty, TEmpty), TNode(113, TEmpty, TEmpty, TEmpty)), TNode(12, TNode(121, TEmpty, TEmpty, TEmpty), TEmpty, TNode(122, TEmpty, TEmpty, TEmpty)), TEmpty);;
let t2 = TEmpty;;
let t3 = TNode (1, TEmpty, TEmpty, TEmpty);;

mapTree3 (fun x -> x * 10) t1;;
mapTree3 (fun x -> x * 10) t2;;
mapTree3 (fun x -> x * 10) TEmpty;;
mapTree3 (fun x -> x * 10) t3;;

(* 2 *)


type file = File of string
type dir = Dir of string * (dir list) * (file list) 
type drive = Drive of char * (dir list) * (file list)

let path filesys name = 
  let rec fileSearch queue =
    match queue with
     [] -> None
    | h::t -> 
      let File(n) = h in
      if name = n then Some n else fileSearch t
  in 
  let rec dirSearch queue =
    match queue with
    [] -> None
    | h::t ->
    let Dir(n, ds, fs) = h in 
    if name = n then Some (n ^ "\\")else 
      match fileSearch fs with
      | Some f -> Some ( n ^ "\\" ^ f)
      | None -> (
          match dirSearch t with
          | Some d -> Some d
          | None -> (
            match dirSearch ds with
            | Some d -> Some (n ^ "\\" ^ d)
            | None -> None
          )
        )
  in
  let Drive(c, ds, fs) = filesys in 
  match fileSearch fs with
  None -> (
    match dirSearch ds with
    None -> None
    | Some d -> Some (String.make 1 c ^ ":\\" ^ d)
  )
  | Some f -> Some (String.make 1 c ^ ":\\" ^  f)
;;


let d1 = Drive('C', [Dir("dir1", [Dir("dir2", [], [File("file1") ; File("file5")])], [File("file4")]) ; Dir("dir3", [], [File("file1")])], [File("file2") ; File("file4")]);;

path d1 "file1";;
path d1 "file2";;
path d1 "dir1";;
path d1 "dir2";;
path d1 "file3";;
path d1 "dir3";;
path d1 "file4";;
path d1 "file5";;
path d1 "dir4";;