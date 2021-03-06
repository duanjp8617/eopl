open Syntax
open Translator
   
type nl_environment = expval list

and expval =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of nl_expression * (nl_environment ref)
             
let string_of_expval value =
  match value with
  | NumVal n -> string_of_int n
  | BoolVal b -> string_of_bool b
  | ProcVal _ -> "proc"
             
let empty_nl_env () = []

let extend_nl_env value env = value :: env

exception MissInEnv of int
                                  
let rec apply_nl_env num env =
  match env with
  | x :: ls ->
     (match num with
      | 0 -> x
      | n -> apply_nl_env (n-1) ls)
  | [] -> raise (MissInEnv num)

exception InterpreterError of string * Ploc.t

let rec eval_nl_exp exp env =
  match exp with
  | NlConstExp (num, loc) -> NumVal num
  | NlDiffExp (exp1, exp2, loc) ->
     let exp_val1 = eval_nl_exp exp1 env in
     let exp_val2 = eval_nl_exp exp2 env in
     (match (exp_val1, exp_val2) with
      | (NumVal num1, NumVal num2) -> NumVal (num1-num2) 
      | _ -> raise (InterpreterError ("DiffExp error", loc)))
  | NlIsZeroExp (exp, loc) ->
     let exp_val = eval_nl_exp exp env in
     (match exp_val with
      | NumVal num -> if num = 0 then BoolVal true else BoolVal false
      | _ -> raise (InterpreterError ("IsZeroExp error", loc)))
  | NlIfExp (exp1, exp2, exp3, loc) ->
     let exp_val1 = eval_nl_exp exp1 env in
     (match exp_val1 with
      | BoolVal b -> if b then eval_nl_exp exp2 env else eval_nl_exp exp3 env
      | _ -> raise (InterpreterError ("IfExp error", loc)))
  | NlVarExp (num, loc) -> 
     (try apply_nl_env num env
      with MissInEnv err_num -> raise (InterpreterError ("Can not find variable " ^ (string_of_int err_num) ^ " in environment", loc)))
  | NlLetExp (exp1, exp2, loc) ->
     (let new_env = extend_nl_env (eval_nl_exp exp1 env) env in
      eval_nl_exp exp2 new_env)
  | NlProcExp (exp, pos_list, loc) ->
     Printf.printf("Build a ProcVal.\n");
     ProcVal (exp, ref (retrieve_new_env env pos_list))
  | NlApplyExp (pbody, pos_list, exp_list, loc) ->
     let new_proc_env = List.append
                          (List.map (fun nl_exp -> eval_nl_exp nl_exp env) exp_list)
                          (retrieve_new_env env pos_list) in
     eval_nl_exp pbody new_proc_env
      

let eval_top_level (ExpTop e) =
  eval_nl_exp (translate_of e (empty_env ())) (empty_nl_env ())|> string_of_expval |> print_endline
                                                     
let value_of_program (AProgram tl_list) = List.iter eval_top_level tl_list 
