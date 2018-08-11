open Syntax

type nl_expression =
  | NlConstExp of int * Ploc.t
  | NlDiffExp of nl_expression * nl_expression * Ploc.t
  | NlIsZeroExp of nl_expression * Ploc.t
  | NlIfExp of nl_expression * nl_expression * nl_expression * Ploc.t
  | NlVarExp of int * Ploc.t
  | NlLetExp of nl_expression * nl_expression * Ploc.t
  | NlProcExp of nl_expression * (int list) * Ploc.t
  | NlApplyExp of nl_expression * (nl_expression list) * Ploc.t

type env_var =
  | VarName of string
  | ProcExp of string * nl_expression * (int list)

and environment = env_var list

                
val empty_env : unit -> environment

val translate_of : expression -> environment -> nl_expression                          

val retrieve_new_env : 'a list -> int list -> 'a list
