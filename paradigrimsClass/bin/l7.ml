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