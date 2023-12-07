 
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

(*
type file = File of string
type dir = Dir of string * (dir list) * (file list) 
type drive = Drive of char * (dir list) * (file list)
*)

type obj = File of string | Dir of string * (obj list)
type drive = Drive of char * (obj list)

(*
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
*)



let path filesys name =
  let rec listSearch ls = 
    let rec objSearch o =
      match o with
      | File(n) -> if n = name then ("\\", 0) else ("", 0)
      | Dir(n,os) -> if n = name then ("\\", 0) else 
        let (p, d) = listSearch os in 
        if p = "" then ("", 0) else 
          if p = "\\" then (n ^ p, d + 1)
          else (n ^ "\\" ^ p, d + 1)
    in
    match ls with
    | [] -> ("", 0)
    | h::t ->
      (
        let (hp, hd) = objSearch h and (tp, td) = listSearch t in
        if hp = "" && tp = "" then ("", 0)
        else if hp = "" then (tp, td)
        else if tp = "" then (hp, hd)
        else if hd < td then (hp, hd)
        else (tp, td)
      ) 
  in 
  let Drive(c, os) = filesys in
    let (p, d) = listSearch os in
    if p = "" then None else Some (String.make 1 c ^ ":" ^ p)
;;

let d1 = Drive('C', [Dir("dir1", [Dir("dir2", [Dir("dir7", [File("file101")]) ; File("file1") ; File("file5")]) ; File("file4")]) ; Dir("dir3", [Dir ("dir6", [File("file101")]) ; File("file1")]) ;File("file2") ; File("file4")]);;
 let d2 = Drive('D', [Dir("dir1", [Dir("dir2", [File("file")])]); Dir("dir3", [Dir("dir4", [Dir("dir5", [File("file")])])])]);; 


path d1 "file1";;
path d1 "file2";;
path d1 "dir1";;
path d1 "dir2";;
path d1 "file3";;
path d1 "dir3";;
path d1 "file4";;
path d1 "file5";;
path d1 "dir4";;
path d1 "file101";;



path d2 "file";; 



let insert drive path obj = 
  let rec addFile remainingPath inDir  =
    let Dir(n, objList) = inDir in
    match remainingPath with
    | [] -> Dir(n, obj::objList)
    | h::t -> 
      (
        let rec findDir toFind dirs = 
          match dirs with
          [] -> [addFile t (Dir(toFind, []))]
          | hDirs::tDirs -> (
            match hDirs with
            Dir(dn, _) when dn = toFind  -> [addFile t hDirs]
            |_ -> (findDir toFind tDirs)
          )
        in 
        let rec newDirs duplName newD oldDs = 
          match oldDs with
          [] -> []
          | odH::odT ->
            (
              match odH with
              Dir(nam, _) when duplName = nam -> newDirs duplName newD odT
              | _ -> odH::(newDirs duplName newD odT)
            )
          in
          let newD = findDir h objList in
        Dir( n, newD@(newDirs h newD objList ))
      )
      in 
        let Drive(c, subs) = drive 
      in
        let fake = Dir("fake", subs)      
      in 
        let Dir(_, res) = addFile path fake 
      in 
            Drive(c, res)
;;



let pat1 = ["dir8"; "dir2"];;

let pat2 = ["dir1"; "dir2"];;

let pat3 = ["dir3"; "dir2"];;

insert d1 pat1 (Dir("NEW", []));;

insert d1 pat2 (Dir("NEW", [File("NNNN")]));;

insert d1 pat3 (File("NEW"));;

insert (Drive('A', [])) ["one"; "two"] (File("LOooL"));;

