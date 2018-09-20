open Syntax

type environment = (string * expval) list

and expval =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of string * expression * (environment ref)

let string_of_expval value =
  match value with
  | NumVal n -> string_of_int n
  | BoolVal b -> string_of_bool b
  | ProcVal _ -> "proc"
             
let empty_env () = []

let extend_env variable value env = (variable, value) :: env
                                  
exception MissInEnv of string

                     
let rec apply_env variable env =
  match env with
  | (vname, vvalue) :: tl ->
     if vname = variable
     then vvalue
     else apply_env variable tl
  | [] ->
     raise (MissInEnv variable)

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
     ProcVal (str, exp, (ref env))
  | ApplyExp (str, exp, loc) ->
     (let proc = apply_env str env in
      match proc with
      | ProcVal (arg_name, proc_body, proc_env_ref) ->
         (let new_proc_env = extend_env arg_name (eval_exp exp env) !proc_env_ref in
          eval_exp proc_body new_proc_env)
      | _ -> raise (InterpreterError ("proc is not defined", loc)))
  | ProcDefExp (_, _, _, loc) -> raise (InterpreterError ("we don't evaluate procedure definition", loc))
  | LetRecExp (ls, exp, loc) ->
     let env_ref = ref [] in
     let proc_list = List.map
                       (fun exp ->
                         match exp with
                         | ProcDefExp (proc_name, proc_var_name, proc_body, ploc) ->
                            (proc_name, ProcVal (proc_var_name, proc_body, env_ref))
                         | _ -> raise (InterpreterError ("Impossible error.", loc)))
                       ls in
     let new_env = List.append proc_list env in
     env_ref := new_env;
     eval_exp exp new_env
     
    
let eval_top_level (ExpTop e) =
  eval_exp e (empty_env ()) |> string_of_expval |> print_endline
                                                     
let value_of_program (AProgram tl_list) = List.iter eval_top_level tl_list 
