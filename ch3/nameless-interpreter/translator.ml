open Syntax

type environment = (string list) list

exception MissInEnv of string
                 
let empty_env () = []

let extend_env ls env = ls :: env

let search_in_list str str_ls =
  let rec do_search str str_ls =
    match str_ls with
    | [] -> raise (MissInEnv str)
    | hd :: tl ->
       if str = hd
       then 0
       else 1 + (do_search str tl)
  in
  try Some (do_search str str_ls) with
  | MissInEnv _ -> None
    
                      
let apply_env variable env =
  let rec do_apply_env variable env fst_pos =
    match env with
    | [] -> raise (MissInEnv variable)
    | h_ls :: tail_ls ->
       match (search_in_list variable h_ls) with
       | None -> do_apply_env variable tail_ls (fst_pos + 1)
       | Some n -> (fst_pos, n)
  in
  do_apply_env variable env 0
     

type nl_expression =
  | NlConstExp of int * Ploc.t
  | NlDiffExp of nl_expression * nl_expression * Ploc.t
  | NlIsZeroExp of nl_expression * Ploc.t
  | NlIfExp of nl_expression * nl_expression * nl_expression * Ploc.t
  | NlVarExp of (int*int) * Ploc.t
  | NlLetExp of nl_expression * nl_expression * Ploc.t
  | NlProcExp of nl_expression * Ploc.t
  | NlApplyExp of nl_expression * (nl_expression list) * Ploc.t
  | NlLetRecExp of nl_expression * nl_expression * Ploc.t
                
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
     NlLetExp ((translate_of exp1 env), (translate_of exp2 (extend_env (str :: []) env)), loc)
  | ProcExp (str_ls, exp, loc) ->
     NlProcExp ((translate_of exp (extend_env str_ls env)), loc)
  | ApplyExp (exp1, exp_ls, loc) ->
     NlApplyExp ((translate_of exp1 env), (List.map (fun exp -> translate_of exp env) exp_ls), loc)
  | LetRecExp (var1, str_ls, exp1, exp2, loc) ->
     NlLetRecExp ( (translate_of exp1 (extend_env str_ls (extend_env (var1 :: []) env))),
                    (translate_of exp2 (extend_env (var1 :: []) env)), loc)
