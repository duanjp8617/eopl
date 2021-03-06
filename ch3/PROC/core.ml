open Syntax

type environment = (string * expval) list

and expval =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of (string list) * expression
  | TraceProcVal of (string list) * expression * environment
             
let string_of_expval value =
  match value with
  | NumVal n -> string_of_int n
  | BoolVal b -> string_of_bool b
  | ProcVal _ -> "proc"
  | TraceProcVal _ -> "proc"
             
let empty_env () = []

let extend_env variable value env = (variable, value) :: env

exception MissInEnv of string
                                  
let rec apply_env variable env =
  match env with
  | (var, value) :: tl -> if variable = var then value else apply_env variable tl
  | [] -> raise (MissInEnv variable)

exception InterpreterError of string * Ploc.t

let string_of_arg_names proc_name arg_names =
  ("(" ^ proc_name ^ (List.fold_left (fun accu name -> accu ^ " " ^ name) "" arg_names) ^ ")")
  
let print_proc prefix proc_name arg_names =
  print_endline (prefix ^ " " ^ (string_of_arg_names proc_name arg_names))
  
let rec find_proc_name exp env =
  match exp with
  | VarExp (str, _) -> str
  | ApplyExp (exp1, _, _) -> find_proc_name exp1 env
  | _ -> "wtf?"
  
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
  | ProcExp (ls, exp, loc) ->
     ProcVal (ls, exp)
  | TraceProcExp (ls, exp, loc) ->
     TraceProcVal (ls, exp, env)
  | ApplyExp (exp1, exp_ls, loc) ->
     let proc = eval_exp exp1 env in
     match proc with
     | TraceProcVal (arg_names, proc_body, proc_env) ->
        print_proc "call" (find_proc_name exp1 env) arg_names;
        let arg_exp_ls = List.combine arg_names exp_ls in
        let new_proc_env = List.fold_left
                             (fun cur_env (arg_name, arg_exp) ->
                               extend_env arg_name (eval_exp arg_exp env) cur_env)
                             proc_env arg_exp_ls in
        let res = eval_exp proc_body new_proc_env in
        (print_proc "exit" (find_proc_name exp1 env) arg_names; res)
     | ProcVal (arg_names, proc_body) ->
        let arg_exp_ls = List.combine arg_names exp_ls in
        let new_proc_env = List.fold_left
                             (fun cur_env (arg_name, arg_exp) ->
                               extend_env arg_name (eval_exp arg_exp env) cur_env)
                             env arg_exp_ls in
        eval_exp proc_body new_proc_env 
     | _ -> raise (InterpreterError ("proc is not defined", loc))
     
let eval_top_level (ExpTop e) =
  eval_exp e (empty_env ()) |> string_of_expval |> print_endline
                                                     
let value_of_program (AProgram tl_list) = List.iter eval_top_level tl_list 
