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
  | NlProcExp of nl_expression * (int list)  *Ploc.t
  | NlApplyExp of nl_expression * (nl_expression list) * Ploc.t
  | NlLetRecExp of nl_expression * (int list) * nl_expression * Ploc.t

let gen_pos_list n =
  let rec do_gen l =
    match l with
    | hd :: tl ->
       if hd <> 0
       then do_gen ((hd-1) :: l)
       else l
    | [] -> []
  in do_gen [n-1]

let retrieve_new_env env pos_list =
  let rec do_retrieve_one env pos =
    match env with
    | hd :: tl ->
       if pos <> 0
       then do_retrieve_one tl (pos-1)
       else hd
    | [] -> raise (MissInEnv "retrieve_new_env fail")
  in List.map (fun pos -> do_retrieve_one env pos) pos_list
   
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
  | ProcExp (str_list, exp, loc) ->
     let env_with_proc_arg = List.append str_list env in 
     let pos_list = gen_pos_list (List.length env_with_proc_arg) in
     let new_env = retrieve_new_env env_with_proc_arg pos_list in 
     NlProcExp ((translate_of exp new_env),pos_list, loc)
  | ApplyExp (exp1, exp_ls, loc) ->
     NlApplyExp ((translate_of exp1 env), List.map (fun exp -> translate_of exp env) exp_ls, loc)
  | LetRecExp (var1, str_list, exp1, exp2, loc) ->
     let env_with_proc_arg = List.append str_list (extend_env var1 env) in
     let pos_list = gen_pos_list (List.length env_with_proc_arg) in
     let new_env = retrieve_new_env env_with_proc_arg pos_list in
     NlLetRecExp ( (translate_of exp1 new_env), pos_list,
                   (translate_of exp2 (extend_env var1 env)), loc)
