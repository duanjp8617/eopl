open Syntax

type environment =
  | EmptyEnv
  | ExtendEnv of string * expval * environment
  | ExtendEnvRec of (expression list) * environment

and expval =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of string * expression * environment

let string_of_expval value =
  match value with
  | NumVal n -> string_of_int n
  | BoolVal b -> string_of_bool b
  | ProcVal _ -> "proc"
             
let empty_env () = EmptyEnv

let extend_env variable value env = ExtendEnv (variable, value, env)

let extend_env_rec exp_ls env = ExtendEnvRec (exp_ls, env)
                                  
exception MissInEnv of string

let rec get_proc variable exp_ls =
  match exp_ls with
  | (ProcDefExp (p_name, v_name, p_body, loc) :: ls) ->
     if p_name = variable
     then Some (v_name, p_body)
     else get_proc variable ls
  | _ :: ls -> raise (MissInEnv variable)
  | [] -> None
                     
let rec apply_env variable env =
  match env with
  | EmptyEnv -> raise (MissInEnv variable)
  | ExtendEnv (v_name, v_value, cur_env) ->
     (if v_name = variable
      then v_value
      else apply_env variable cur_env)
  | ExtendEnvRec (exp_ls, p_env) ->
     match get_proc variable exp_ls with
     | Some (v_name, p_body) -> ProcVal (v_name, p_body, env)
     | None -> apply_env variable p_env

exception InterpreterError of string * Ploc.t
                            
let rec eval_exp exp env =
  match exp with
  | ConstExp (num, loc) -> NumVal num
  | DiffExp (exp1, exp2, loc) ->
     let exp_val1 = eval_exp exp1 env in
     let exp_val2 = eval_exp exp2 env in
     (match (exp_val1, exp_val2) with
      | (NumVal num1, NumVal num2) -> NumVal (num1-num2) 
      | _ -> raise (InterpreterError ("DiffExp error", loc)))
  | IsZeroExp (exp, loc) ->
     let exp_val = eval_exp exp env in
     (match exp_val with
      | NumVal num -> if num = 0 then BoolVal true else BoolVal false
      | _ -> raise (InterpreterError ("IsZeroExp error", loc)))
  | IfExp (exp1, exp2, exp3, loc) ->
     let exp_val1 = eval_exp exp1 env in
     (match exp_val1 with
      | BoolVal b -> if b then eval_exp exp2 env else eval_exp exp3 env
      | _ -> raise (InterpreterError ("IfExp error", loc)))
  | VarExp (str, loc) -> 
     (try apply_env str env
      with MissInEnv err_msg -> raise (InterpreterError ("Can not find variable " ^ err_msg ^ " in environment", loc)))
  | LetExp (str, exp1, exp2, loc) ->
     (let new_env = extend_env str (eval_exp exp1 env) env in
      eval_exp exp2 new_env)
  | ProcExp (str, exp, loc) ->
     ProcVal (str, exp, env)
  | ApplyExp (str, exp, loc) ->
     (let proc = apply_env str env in
      match proc with
      | ProcVal (arg_name, proc_body, proc_env) ->
         (let new_proc_env = extend_env arg_name (eval_exp exp env) proc_env in
          eval_exp proc_body new_proc_env)
      | _ -> raise (InterpreterError ("proc is not defined", loc)))
  | ProcDefExp (_, _, _, loc) -> raise (InterpreterError ("we don't evaluate procedure definition", loc))
  | LetRecExp (ls, exp, loc) ->
     (let new_env = extend_env_rec ls env in
      eval_exp exp new_env)
let eval_top_level (ExpTop e) =
  eval_exp e (empty_env ()) |> string_of_expval |> print_endline
                                                     
let value_of_program (AProgram tl_list) = List.iter eval_top_level tl_list 
