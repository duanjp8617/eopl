open Syntax

val value_of_program : program -> unit

exception InterpreterError of string * Ploc.t                                    
