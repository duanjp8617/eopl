open Syntax

type environment = string list

exception MissInEnv of string
                 
let empty_env () = []

let extend_env variable env = variable :: env

let rec apply_env variable env =
  match env with
  | [] -> raise (MissInEnv variable)
  | x :: ls ->
     if variable = x
     then 0
     else 1 + (apply_env variable ls)

type nl_expression =
  | NlConstExp of int * Ploc.t
  | NlDiffExp of nl_expression * nl_expression * Ploc.t
  | NlIsZeroExp of nl_expression * Ploc.t
  | NlIfExp of nl_expression * nl_expression * nl_expression * Ploc.t
  | NlVarExp of int * Ploc.t
  | NlLetExp of nl_expression * nl_expression * Ploc.t
  | NlProcExp of nl_expression * Ploc.t
  | NlApplyExp of nl_expression * nl_expression * Ploc.t

let rec translate_of exp env =
  match exp with
  | ConstExp (num, loc) ->
     NlConstExp (num, loc)
  | DiffExp (exp1, exp2, loc) ->
     NlDiffExp ((translate_of exp1 env), (translate_of exp2 env), loc)
  | IsZeroExp (exp, loc) ->
     NlIsZeroExp (translate_of exp env, loc)
  | IfExp (exp1, exp2, exp3, loc) ->
     NlIfExp ((translate_of exp1 env), (translate_of exp2 env), (translate_of exp3 env), loc)
  | VarExp (str, loc) -> 
     NlVarExp ((apply_env str env), loc)
  | LetExp (str, exp1, exp2, loc) ->
     NlLetExp ((translate_of exp1 env), (translate_of exp2 (extend_env str env)), loc)
  | ProcExp (str, exp, loc) ->
     NlProcExp ((translate_of exp (extend_env str env)), loc)
  | ApplyExp (exp1, exp2, loc) ->
     NlApplyExp ((translate_of exp1 env), (translate_of exp2 env), loc)
