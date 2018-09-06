open Syntax

type environment = (string * int) list

and expval =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of string * expression * (environment ref)
  | MutPairVal of int * int 

and answer =
  | Answer of expval * ((expval ref) list)
            
let string_of_expval value =
  match value with
  | Answer (NumVal n, _) -> string_of_int n
  | Answer (BoolVal b, _) -> string_of_bool b
  | Answer (ProcVal _, _) -> "proc"
  | Answer (MutPairVal _, _) -> "mutable_pair"
             
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

let empty_store () = []

let new_ref value store =
  let ref_id = List.length store in
  let new_store = store @ [ref value] in
  (ref_id, new_store)

let rec do_find id lst =
    match lst with
    | hd :: tl ->
       if id = 0
       then hd
       else do_find (id-1) tl
    | [] -> raise (InvalidReferenceError id)
  
let deref ref_value store =
  !(do_find ref_value store)
  

let set_ref ref_value value store = 
  (do_find ref_value store) := value
  
(* The core eval_exp function  *)  
let rec eval_exp exp env store =
  match exp with
  | ConstExp (num, loc) -> Answer (NumVal num, store)
  | DiffExp (exp1, exp2, loc) ->
     let Answer (exp_val1, store1) = eval_exp exp1 env store in
     let Answer (exp_val2, store2) = eval_exp exp2 env store1  in
     (match (exp_val1, exp_val2) with
      | (NumVal num1, NumVal num2) -> Answer (NumVal (num1-num2), store2) 
      | _ -> raise (InterpreterError ("DiffExp error", loc)))
  | IsZeroExp (exp, loc) ->
     let Answer (exp_val, store1) = eval_exp exp env store in
     (match exp_val with
      | NumVal num -> if num = 0 then Answer (BoolVal true, store1) else Answer (BoolVal false, store1)
      | _ -> raise (InterpreterError ("IsZeroExp error", loc)))
  | IfExp (exp1, exp2, exp3, loc) ->
     let Answer (exp_val1, store1) = eval_exp exp1 env store in
     (match exp_val1 with
      | BoolVal b -> if b then eval_exp exp2 env store1 else eval_exp exp3 env store1
      | _ -> raise (InterpreterError ("IfExp error", loc)))
  | VarExp (str, loc) -> 
     (try Answer ((deref (apply_env str env) store), store)
      with MissInEnv err_msg -> raise (InterpreterError ("Can not find variable " ^ err_msg ^ " in environment", loc)))
  | LetExp (str, exp1, exp2, loc) ->
     (let Answer (exp_val1, store1) = eval_exp exp1 env store in
      let (ref_val, store2) = new_ref exp_val1 store1 in
      let new_env = extend_env str ref_val env in
      eval_exp exp2 new_env store2)
  | ProcExp (str, exp, loc) ->
     Answer (ProcVal (str, exp, (ref env)), store)
  | ApplyExp (str, exp, loc) ->
     (match deref (apply_env str env) store with
      | ProcVal (arg_name, proc_body, proc_env_ref) ->
         (let Answer (exp_val, store2) = eval_exp exp env store in
          let (ref_val, store3) = new_ref exp_val store2 in 
          let new_proc_env = extend_env arg_name ref_val !proc_env_ref in
          eval_exp proc_body new_proc_env store3)
      | _ -> raise (InterpreterError ("proc is not defined", loc)))
  | ProcDefExp (_, _, _, loc) -> raise (InterpreterError ("we don't evaluate procedure definition", loc))
  | LetRecExp (ls, exp, loc) ->
     let env_ref = ref [] in
     let (store1, proc_ref_list) = List.fold_left
                                     (fun (accum_store, ref_list) exp ->
                                       match exp with
                                       | ProcDefExp (proc_name, proc_var_name, proc_body, ploc) ->
                                          let (ref_val, new_store) =
                                            new_ref
                                              (ProcVal (proc_var_name, proc_body, env_ref))
                                              accum_store in
                                          (new_store, (proc_name, ref_val) :: ref_list)
                                       |_ -> raise (InterpreterError ("Impossible error.", loc))
                                     )
                                     (store, []) ls in
     let new_env = List.append proc_ref_list env in
     env_ref := new_env;
     eval_exp exp new_env store1
  | BeginEndExp (exp_ls, loc) ->
     let rec iterate exp_ls store =
       match exp_ls with
       | hd :: [] -> eval_exp hd env store
       | hd :: tl ->
          let Answer (exp_val, new_store) = eval_exp hd env store in 
          iterate tl new_store
       | [] -> raise (InterpreterError ("impossible state", loc))
     in
     iterate exp_ls store
  | SetExp (var_name, exp, loc) ->
     let Answer (exp_val, store1) = eval_exp exp env store in
     set_ref (apply_env var_name env) exp_val store1;
     Answer (NumVal 250, store1)
  | SetDynamicExp (var, temp_exp, body, loc) ->
     let old_val_ref = apply_env var env in 
     let old_val = deref old_val_ref store in
     let Answer (temp_val, store1) = eval_exp temp_exp env store in
     let _ = set_ref old_val_ref temp_val store1 in
     let Answer (res_val, store2) = eval_exp body env store1 in
     let _ = set_ref old_val_ref old_val store2 in
     Answer (res_val, store2)
  | NewPairExp (exp1, exp2, loc) ->
     let Answer (exp_val1, store1) = eval_exp exp1 env store in
     let Answer (exp_val2, store2) = eval_exp exp2 env store1 in
     let (ref_val1, store3) = new_ref exp_val1 store2 in
     let (ref_val2, store4) = new_ref exp_val2 store3 in
     Answer (MutPairVal (ref_val1, ref_val2), store4)
  | LeftExp (exp, loc) ->
     let Answer (exp_val, store1) = eval_exp exp env store in
     (match exp_val with
      | MutPairVal (l, r) -> Answer (deref l store1, store1)
      | _ ->raise (InterpreterError ("left is expecting a mutable pair.", loc)))
  | RightExp (exp, loc) ->
     let Answer (exp_val, store1) = eval_exp exp env store in
     (match exp_val with
      | MutPairVal (l, r) -> Answer (deref r store1, store1)
      | _ -> raise (InterpreterError ("right is expecting a mutable pair.", loc)))
  | SetLeftExp (exp1, exp2, loc) ->
     let Answer (mp_val, store1) = eval_exp exp1 env store in
     let Answer (some_val, store2) = eval_exp exp2 env store1 in
     (match mp_val with
      | MutPairVal (l,r) -> set_ref l some_val store2; Answer (NumVal 23, store2)
      | _ -> raise (InterpreterError ("setleft is expecting a mutable pair.", loc)))
  | SetRightExp (exp1, exp2, loc) ->
     let Answer (mp_val, store1) = eval_exp exp1 env store in
     let Answer (some_val, store2) = eval_exp exp2 env store1 in
     (match mp_val with
      | MutPairVal (l,r) -> set_ref r some_val store2; Answer (NumVal 23, store2)
      | _ -> raise (InterpreterError ("setright is expecting a mutable pair.", loc)))
    
     
let eval_top_level (ExpTop e) =
  eval_exp e (empty_env ()) (empty_store ()) |> string_of_expval |> print_endline
                                                     
let value_of_program (AProgram tl_list) = List.iter eval_top_level tl_list 