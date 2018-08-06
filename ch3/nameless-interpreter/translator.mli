open Syntax

type environment = string list

type nl_expression =
  | NlConstExp of int * Ploc.t
  | NlDiffExp of nl_expression * nl_expression * Ploc.t
  | NlIsZeroExp of nl_expression * Ploc.t
  | NlIfExp of nl_expression * nl_expression * nl_expression * Ploc.t
  | NlVarExp of int * Ploc.t
  | NlLetExp of nl_expression * nl_expression * Ploc.t
  | NlProcExp of nl_expression * Ploc.t
  | NlApplyExp of nl_expression * (nl_expression list) * Ploc.t
  | NlLetRecExp of nl_expression * nl_expression * Ploc.t
                
val empty_env : unit -> string list

val translate_of : expression -> string list -> nl_expression                          
