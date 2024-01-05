
module type COPROCESSOR = sig
  type 'float t
  type instruction = Rst | LoadF of float | LoadI of int | Cpy | Add | Sub | Mul | Div

  exception DivisionByZero of string
  exception MissingArgument of string 

  val init: unit -> float t
  val result: float t -> float
  val execute: float t -> instruction list -> unit
end;;


module StackMachine = struct
  type 'float t = {mutable stack : 'float list }
  type instruction = Rst | LoadF of float | LoadI of int | Cpy | Add | Sub | Mul | Div
  
  exception DivisionByZero of string
  exception MissingArgument of string 

  let operation sm oper = 
    match sm.stack with
        o::t::r -> 
          ( 
            if (oper 6.0 2.0 = (/.) 6.0 2.0) && t = 0.0 then raise (DivisionByZero "cannot divide by zero!")
            else sm.stack <- (oper o t)::r
          )
        | _ -> raise (MissingArgument "not enough arguments!") 

  let helper sm i =
    match i with
      Rst ->  sm.stack <- [] 
      | LoadF f -> sm.stack <- f::sm.stack
      | LoadI i -> sm.stack <- (float_of_int i)::sm.stack
      | Cpy -> (match sm.stack with
        [] -> raise (MissingArgument "stack is empty!") 
        | h::_ -> sm.stack <- h::sm.stack )
      | Add -> operation sm (+.)
      | Sub -> operation sm (-.)
      | Mul -> operation sm ( *. )
      | Div -> operation sm (/.)
  

  let init () = { stack = [] }
  let result sm = 
    match sm.stack with 
      [] -> raise (MissingArgument "stack is empty!") 
      | h::_ -> h
  let execute sm ls = 
    List.iter (fun instr -> helper sm instr) ls
end;;

module Coprocessor : COPROCESSOR = StackMachine;;
  

let coproc = Coprocessor.init();;
let i0 = let open Coprocessor in [LoadF 4.5 ; LoadI 4 ; LoadI 2 ; Add ; Sub ; LoadI 1; Div ; Cpy ; Mul];;
let i1 = let open Coprocessor in [Rst ; LoadF 4.5 ; LoadI 4 ; LoadI 2 ; Add ; Mul ; Sub];;
let i2 = let open Coprocessor in [Rst ; LoadF 6.0 ; LoadI 4 ; LoadI 2 ; Add ; Sub ; LoadI 1; Div ; Cpy ; Mul];;
let i3 = let open Coprocessor in [Rst ; LoadI 1; LoadF 6.0 ; LoadI 4 ; LoadI 2 ; Add ; Sub ; Div ; Cpy ; Mul];;
let i4 = let open Coprocessor in [Rst ;  Cpy ];;

let open Coprocessor in
execute coproc i0;
result coproc;;
(* 
let open Coprocessor in
execute coproc i1;
result coproc;; *)

let open Coprocessor in
execute coproc i3;
result coproc;;
(* 
let open Coprocessor in
execute coproc i2;
result coproc;; *)

let open Coprocessor in
execute coproc i4;
result coproc;;