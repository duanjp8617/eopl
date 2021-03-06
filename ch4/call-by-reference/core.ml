open Syntax
   
type environment = (string * int) list

and expval =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of string * expression * (environment ref)
  | MutArrayVal of int * int 

and answer =
  | Answer of expval * ((expval ref) list)
            
let string_of_expval value =
  match value with
  | Answer (NumVal n, _) -> string_of_int n
  | Answer (BoolVal b, _) -> string_of_bool b
  | Answer (ProcVal _, _) -> "proc"
  | Answer (MutArrayVal _, _) -> "mutable_array"
             
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
  | ApplyExp (f_exp, exp, loc) ->
     (match (eval_exp f_exp env store) with
      | Answer (ProcVal (arg_name, proc_body, proc_env_ref), store1) ->
         (let (ref_val, store3) = value_of_oprand exp env store1 in 
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
  | NewArrayExp (exp_ls, loc) ->
     let (exp_val_ls, store1) =
       List.fold_left
         (fun (ls, store) exp ->
           let Answer (exp_val, store1) = eval_exp exp env store in
           (ls @ [exp_val], store1))
         ([], store)
         exp_ls in
     let (last_val_ref, final_store) =
       List.fold_left
         (fun (_, store) exp_val -> new_ref exp_val store)
         (0, store1) exp_val_ls in 
     let len = List.length exp_ls in
     Printf.printf "%d, %d\n" ((last_val_ref - len)+1) len;
     Answer (MutArrayVal (((last_val_ref - len) + 1), len), final_store)
  | ArrayRefExp (exp1, exp2, loc) ->
     let Answer (ar_val, store1) = eval_exp exp1 env store in
     let Answer (pos_val, store2) = eval_exp exp2 env store1 in
     (match (ar_val, pos_val) with
      | (MutArrayVal (first_pos, len), NumVal pos) ->
         (if (pos >= 0) && (pos < len) 
          then Answer (deref (first_pos+pos) store2, store2)
          else raise (InterpreterError ("array ref out of bound." ,loc)))
      | _ -> raise (InterpreterError ("arrayref is expecting an array and an integer.", loc)))
  | ArraySetExp(exp1, exp2, exp3, loc) ->
     let Answer (ar_val, store1) = eval_exp exp1 env store in
     let Answer (pos_val, store2) = eval_exp exp2 env store1 in
     let Answer (final_val, store3) = eval_exp exp3 env store2 in
     (match (ar_val, pos_val) with
      | (MutArrayVal (first_pos, len), NumVal pos) ->
         (if (pos >=0) && (pos < len)
          then
            (set_ref (first_pos+pos) final_val store3;
             Answer(NumVal 23, store3))
          else raise (InterpreterError ("array set out of bound." ,loc)))
      | _ -> raise (InterpreterError ("arrayset is expecting an array and an integer.", loc)))

and value_of_oprand oprand_exp env store =
  match oprand_exp with
  | VarExp(str, loc) -> ((apply_env str env), store)
  | ArrayRefExp(exp1, exp2, loc) ->
     let Answer(ar_val, store1) = eval_exp exp1 env store in
     let Answer(pos_val, store2) = eval_exp exp2 env store1 in
     (match (ar_val, pos_val) with
      | (MutArrayVal (first_pos, len), NumVal pos) ->
         
         (if (pos >= 0) && (pos < len)
          then ((first_pos+pos), store2)
          else raise (InterpreterError ("array ref out of bound 1." ,loc)))
      | _ -> raise (InterpreterError ("arrayref is expecting an array and an integer 1.", loc)))
  | _ ->
     let Answer(exp_val, store1) = eval_exp oprand_exp env store in
     new_ref exp_val store1

     
let eval_top_level (ExpTop e) =
  eval_exp e (empty_env ()) (empty_store ()) |> string_of_expval |> print_endline
                                                     
let value_of_program (AProgram tl_list) = List.iter eval_top_level tl_list 
