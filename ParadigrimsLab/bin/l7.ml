module type COPROCESSOR = sig
  type 'float t
  type instruction = Rst | LoadF of float | LoadI of int | Cpy | Add | Sub | Mul | Div

  exception DivisionByZero of string
  exception MissingArgument of string 
  val init: unit -> float t
  val result: float t -> float
  val execute: float t -> instruction list -> unit

end;;


module StackMachine =
  struct
  type 'float t = {mutable stack : 'float list }
  
  exception DivisionByZero of string
  exception MissingArgument of string 

  type instruction = Rst | LoadF of float | LoadI of int | Cpy | Add | Sub | Mul | Div

  let init () = { stack = [] }
  let result sm = 
    match sm.stack with 
      [] -> raise (MissingArgument "stack is empty!") 
      | h::_ -> h
  
  let helper sm i =
    match i with
      Rst ->  sm.stack <- [] 
      | LoadF f -> sm.stack <- f::sm.stack
      | LoadI i -> sm.stack <- (float_of_int i)::sm.stack
      | Cpy -> (match sm.stack with
        [] -> raise (MissingArgument "stack is empty!") 
        | h::_ -> sm.stack <- h::sm.stack )
      | Add -> (match sm.stack with
        o::t::r -> sm.stack <- (o +. t)::r
        | _ -> raise (MissingArgument "stack is empty!") )
    


  
  let rec execute sm ls = 
    List.iter (fun instr -> helper sm instr) ls
    
end;;