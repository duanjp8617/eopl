open Syntax

type environment =
  | EmptyEnv
  | ExtendEnv of string * expval * environment
  | ExtendEnvRec of string * string * expression * environment

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

let extend_env_rec p_name v_name p_body env = ExtendEnvRec (p_name, v_name, p_body, env)
                                  
exception MissInEnv of string
                                  
let rec apply_env variable env =
  match env with
  | EmptyEnv -> raise (MissInEnv variable)
  | ExtendEnv (v_name, v_value, cur_env) ->
     (if v_name = variable
      then v_value
      else apply_env variable cur_env)
  | ExtendEnvRec (p_name, v_name, p_body, p_env) ->
     (if p_name = variable
      then ProcVal (v_name, p_body, env)
      else apply_env variable p_env)

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
  | LetRecExp (p_name, v_name, p_body, exp, loc) ->
     (let new_env = extend_env_rec p_name v_name p_body env in
      eval_exp exp new_env)
let eval_top_level (ExpTop e) =
  eval_exp e (empty_env ()) |> string_of_expval |> print_endline
                                                     
let value_of_program (AProgram tl_list) = List.iter eval_top_level tl_list 
