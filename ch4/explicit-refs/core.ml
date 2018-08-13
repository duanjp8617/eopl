open Syntax

type environment = (string * expval) list

and expval =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of string * expression * (environment ref)
  | RefVal of int

let string_of_expval value =
  match value with
  | NumVal n -> string_of_int n
  | BoolVal b -> string_of_bool b
  | ProcVal _ -> "proc"
  | RefVal _ -> "ref"
             
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

(* Global definition of the store and other helpers  *)
exception InvalidReferenceError of int

let the_store = ref []

let get_store () = the_store              

let initialize_store () =
  the_store := [];
  ()

let new_ref value =
  let ref_id = List.length !the_store in
  let new_store = !the_store @ [ref value] in
  the_store := new_store;
  RefVal ref_id

let rec do_find id lst =
    match lst with
    | hd :: tl ->
       if id = 0
       then hd
       else do_find (id-1) tl
    | [] -> raise (InvalidReferenceError id)
  
let deref ref_value  =
  match ref_value with
  | RefVal id -> !(do_find id !the_store)
  | _ -> raise (InvalidReferenceError 1024)

let set_ref ref_value value = 
  match ref_value with
  | RefVal id -> (do_find id !the_store) := value; ()
  | _ -> raise (InvalidReferenceError 1025)

(* The core eval_exp function  *)  
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
  | NewRefExp (exp, loc) ->
     new_ref (eval_exp exp env)
  | DeRefExp (exp, loc) ->
     deref (eval_exp exp env)
  | SetRefExp (exp1, exp2, loc) ->
     set_ref (eval_exp exp1 env) (eval_exp exp2 env);
     NumVal 250
  | BeginEndExp (exp_ls, loc) ->
     let rec iterate exp_ls =
       match exp_ls with
       | hd :: [] -> eval_exp hd env
       | hd :: tl ->
          let dummy = eval_exp hd env in 
          iterate tl
       | [] -> raise (InterpreterError ("impossible state", loc))
     in
     iterate exp_ls
       
let eval_top_level (ExpTop e) =
  eval_exp e (empty_env ()) |> string_of_expval |> print_endline
                                                     
let value_of_program (AProgram tl_list) = List.iter eval_top_level tl_list 
