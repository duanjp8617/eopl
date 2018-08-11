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
  | NlProcExp of nl_expression * (int list) *  Ploc.t
  | NlApplyExp of nl_expression * (nl_expression list) * Ploc.t
   
let retrieve_new_env env pos_list =
  let rec do_retrieve_one env pos =
    match env with
    | hd :: tl ->
       if pos <> 0
       then do_retrieve_one tl (pos-1)
       else hd
    | [] -> raise (MissInEnv "retrieve_new_env fail")
  in List.map (fun pos -> do_retrieve_one env pos) pos_list

let probe_pos exp env =
  let size = List.length env in
  let rec do_probe_pos exp env =
    match exp with
    | ConstExp _ -> []
    | DiffExp (exp1, exp2, _) -> (do_probe_pos exp1 env) @ (do_probe_pos exp2 env)
    | IsZeroExp (exp, _ ) -> do_probe_pos exp env
    | IfExp (exp1, exp2, exp3, _) ->
       (do_probe_pos exp1 env) @ (do_probe_pos exp2 env) @ (do_probe_pos exp3 env)
    | VarExp (str, _) ->
       let env_len = List.length env in
       let pos = apply_env str env in
       (if pos > (env_len-(size+1))
        then
          [pos-(env_len-size)]
        else
          [])
    | LetExp (str, exp1, exp2, _) ->
       (do_probe_pos exp1 env) @ (do_probe_pos exp2 (extend_env str env))
    | ProcExp (str_list, exp, _) ->
       (do_probe_pos exp (List.append str_list env))
    | RecProcDef _ -> raise (MissInEnv "probe_pos error")
    | ApplyExp (exp1, exp_ls, loc) ->
       List.fold_left (fun l exp -> l @ (do_probe_pos exp env)) (do_probe_pos exp1 env) exp_ls
  in
  let l = do_probe_pos exp env in
  let l_set = List.fold_left
                (fun l pos ->
                  if (List.exists (fun n -> if n=pos then true else false) l)
                  then l
                  else pos :: l)
                [] l in
  List.fast_sort
    (fun a b ->
      if a = b then 0
      else if a > b then 1 else -1) l_set
                
     
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
     let pos_list = probe_pos (ProcExp (str_list, exp, loc)) env in 
     NlProcExp (
         (translate_of exp (List.append str_list (retrieve_new_env env pos_list))),
         pos_list,
         loc)
  | RecProcDef _ -> raise (MissInEnv "Invalid translation of RecProcDef")
  | ApplyExp (exp1, exp_ls, loc) ->
     NlApplyExp ((translate_of exp1 env), List.map (fun exp -> translate_of exp env) exp_ls, loc)     
