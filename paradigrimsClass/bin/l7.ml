(* zad 1 *)

module type QUEUE_FUN =
sig
type 'a t
exception Empty of string
val empty: unit -> 'a t
val enqueue: 'a * 'a t -> 'a t
val dequeue: 'a t -> 'a t
val first: 'a t -> 'a
val isEmpty: 'a t -> bool
end;;


module QueueA : QUEUE_FUN = 
struct
  type 'a t = 'a list 

  exception Empty of string
  let empty () = []

  let enqueue (x, l) = l @ [x]

  let dequeue = function
    | [] -> []
    | _ :: l -> l

  let first = function
    | [] -> raise (Empty "Queue is empty")
    | x :: _ -> x

  let isEmpty = function 
    | [] -> true
    | _ -> false

end
;;



module QueueB : QUEUE_FUN = 
struct
  type 'a t = 'a list * 'a list 
  exception Empty of string
  let empty () = ([], [])

  let enqueue (x, l) = 
    match l with
    (h :: t, l2) -> (h::t, x :: l2)
    | ([], l2) -> (List.rev(x::l2), [])

  let dequeue = function
    ([],[]) -> ([],[])
    | (h::[], l2) -> (List.rev l2, [])
    | (h::t, l2) -> (t, l2)

  let first = function
    | ([], []) -> raise (Empty "Queue is empty")
    | (h::t, _) -> h

  let isEmpty = function 
    | ([],[]) -> true
    | _ -> false

end
;;

let open QueueA in 
first(enqueue(4, dequeue(enqueue(1, enqueue(3, dequeue(enqueue(1, empty())))))))
;;

let open QueueA in 
isEmpty(dequeue(enqueue(1, enqueue(3, dequeue(dequeue(enqueue(1, empty())))))))
;;

let open QueueA in 
dequeue(enqueue(1, enqueue(3, dequeue(dequeue(enqueue(1, empty()))))))
;;

let open QueueB in
first(enqueue(4, dequeue(enqueue(1, enqueue(3, dequeue(enqueue(1, empty())))))))
;;

let open QueueB in
isEmpty(dequeue(enqueue(1, enqueue(3, dequeue(dequeue(enqueue(1, empty())))))))
;;

let open QueueB in
enqueue(1, enqueue(3, enqueue(5, dequeue(enqueue(1, empty())))))
;;



(* zad 2 *)
module type QUEUE_MUT =
sig
type 'a t
(* The type of queues containing elements of type ['a]. *)
exception Empty of string
(* Raised when [first q] is applied to an empty queue [q]. *)
exception Full of string
(* Raised when [enqueue(x,q)] is applied to a full queue [q]. *)
val empty: int -> 'a t
(* [empty n] returns a new queue of length [n], initially empty. *)
val enqueue: 'a * 'a t -> unit
(* [enqueue (x,q)] adds the element [x] at the end of a queue [q]. *)
val dequeue: 'a t -> unit
(* [dequeue q] removes the first element in queue [q] *)
val first: 'a t -> 'a
(* [first q] returns the first element in queue [q] without removing
it from the queue, or raises [Empty] if the queue is empty. *)
val isEmpty: 'a t -> bool
(* [isEmpty q] returns [true] if queue [q] is empty,
otherwise returns [false]. *)
val isFull: 'a t -> bool
(* [isFull q] returns [true] if queue [q] is full,
otherwise returns [false]. *)
end;;

module QueueMut : QUEUE_MUT =
struct
  type 'a t = {mutable a: 'a option array; capacity: int; mutable size: int; mutable f: int; mutable r: int}
  exception Empty of string
  exception Full of string

  let empty n = {a = Array.make n None; capacity = n; size = 0;f = 0; r = 0}

  let enqueue(x, q)=
    if q.size = q.capacity then raise (Full "Queue is full")
    else
      q.a.(q.r) <- Some x;
      q.r <- ((q.r + 1) mod q.capacity);
      q.size <- (q.size + 1) 

  let dequeue q =
    if q.size = 0 then ()
    else
    q.a.(q.f) <- None;
    q.f <- ((q.f + 1) mod q.capacity);
    q.size <- (q.size - 1)

  let first q = 
    match q.a.(q.f) with
    | None -> raise (Empty "Queue is empty")
    | Some x -> x

  let isEmpty q = q.size = 0

  let isFull q = q.capacity = q.size
      
end
;;

let open QueueMut in
let q = empty 3 in
enqueue(1, q);
enqueue(2, q);
enqueue(3, q);
dequeue q; 
enqueue(4, q);
first q;;

let open QueueMut in
let q = empty 3 in
(* isFull q; *)
dequeue q;
enqueue(4, q);
dequeue q;
dequeue q;
dequeue q;
(* isEmpty q; *)
dequeue q;
enqueue(4, q);
first q;;


