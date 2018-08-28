open Syntax

(* val value_of_program : program -> unit *)
val value_of_program : statement -> unit
   
exception InterpreterError of string * Ploc.t                                    
