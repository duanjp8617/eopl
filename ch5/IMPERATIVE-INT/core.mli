open Syntax

val value_of_program : program -> unit

exception InterpreterError of string * Ploc.t                                    
exception ApplyContError of string
exception MissInEnv of Ploc.t
