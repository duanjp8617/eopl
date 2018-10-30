open Syntax

type environment = (string * int) list

and expval =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of string * expression * (environment ref)

type continuation =
  | EndCont
  | DiffFstCont of expression * environment * continuation
  | DiffSndCont of expval * continuation 
  | IsZeroCont of continuation 
  | IfCont of expression * expression * environment * continuation 
  | LetCont of string * expression * environment * continuation 
  | ApplyFstCont of expression * environment * continuation
  | ApplySndCont of expval * continuation 
  | MultiLetFstCont of string * environment * (expression list) * expression * Ploc.t * continuation
  | MultiLetSndCont of string * environment * expression * continuation
  | SetCont of string * environment * continuation
  | BeginFstCont of (expression list) * environment * Ploc.t * continuation
  | BeginSndCont of continuation
             
let string_of_expval value =
  match value with
  | NumVal n -> string_of_int n
  | BoolVal b -> string_of_bool b
  | ProcVal _ -> "proc"

exception MissInStore of int
               
let store = ref []

let clear_store () = store := []
          
let newref exp_val =
  let ref_id = List.length !store in
  store := !store @ [ref exp_val];
  ref_id

let rec deref ref_val =
  let rec go ref_val ls =
    match ls with
    | [] -> raise (MissInStore ref_val)
    | hd :: tl ->
       if ref_val = 0 then !hd else (go (ref_val - 1) tl)
  in
  go ref_val !store

let rec setref ref_val exp_val =
  let rec go ref_val exp_val ls =
    match ls with
    | [] -> raise (MissInStore ref_val)
    | hd :: tl ->
       if ref_val = 0 then hd := exp_val else (go (ref_val -1) exp_val tl)
  in
  go ref_val exp_val !store

let empty_env () = []

let extend_env variable value env = (variable, (newref value)) :: env
                                  
exception MissInEnv of string
                     
let rec apply_env variable env =
  match env with
  | (vname, vvalue) :: tl ->
     if vname = variable
     then (deref vvalue)
     else apply_env variable tl
  | [] ->
     raise (MissInEnv variable)

let rec retrieve_ref_val str env =
  match env with
  | [] -> raise (MissInEnv str)
  | (variable, ref_val) :: tl ->
     if variable = str then ref_val else retrieve_ref_val str tl
    
exception InterpreterError of string * Ploc.t
exception ApplyContError of string
                          
let rec eval_exp exp env cont =
  match exp with
  | ConstExp (num, loc) -> apply_cont cont (NumVal num)
  | VarExp (str, loc) -> apply_cont cont (apply_env str env)
  | ProcExp (str, body, loc) -> apply_cont cont (ProcVal (str, body, (ref env)))
  | ProcDefExp (_,_,_,loc) -> raise (InterpreterError ("we don't evaluate ProcDefExp", loc))
  | DiffExp (exp1, exp2, loc) -> eval_exp exp1 env (DiffFstCont (exp2, env, cont))
  | IsZeroExp (exp, loc) -> eval_exp exp env (IsZeroCont cont)
  | IfExp(exp1, exp2, exp3, loc) -> eval_exp exp1 env (IfCont (exp2, exp3, env, cont))
  | LetExp (str, exp1, exp2, loc) -> eval_exp exp1 env (LetCont (str, exp2, env, cont))
  | ApplyExp(exp1, exp2, loc) -> eval_exp exp1 env (ApplyFstCont (exp2, env, cont))
  | LetRecExp(ls, body, loc) ->
     (let env_ref = ref [] in
      let proc_list = List.map
                        (fun exp ->
                          match exp with
                          | ProcDefExp (proc_name, proc_var_name, proc_body, ploc) ->
                             (proc_name, (newref (ProcVal (proc_var_name, proc_body, env_ref))))
                          | _ -> raise (InterpreterError ("Impossible error.", loc)))
                        ls in
      let new_env = List.append proc_list env in
      env_ref := new_env;
      eval_exp body new_env cont)
  | LetDefExp (_,_,loc) -> raise (InterpreterError ("We don't evaluate LetDefExp", loc))
  | MultiLetExp (ls, body, loc) ->
     (match ls with
      | [] -> raise (InterpreterError ("MultiLetExp expects non-empty expression list", loc))
      | hd :: tl ->
         (match tl with
          | [] ->
             (match hd with
              | LetDefExp (str, exp, loc) ->
                 eval_exp exp env (MultiLetSndCont (str, env, body, cont))
              | _ -> raise (InterpreterError ("MultiLetExp expects LetDefExp", loc)))
          | _ -> 
             (match hd with
              | LetDefExp (str, exp, loc) ->
                 eval_exp exp env (MultiLetFstCont (str, env, tl, body, loc, cont))
              | _ -> raise (InterpreterError ("MultiLetExp expects LetDefExp", loc)))))
  | SetExp (str, exp, loc) ->
     eval_exp exp env (SetCont (str, env, cont))
  | BeginExp (exp_ls, loc) ->
     (match exp_ls with
      | [] -> raise (InterpreterError ("BeginExp expects non-empty expression list", loc))
      | hd :: tl ->
         (match tl with
          | [] -> eval_exp hd env (BeginSndCont cont)
          | _ -> eval_exp hd env (BeginFstCont (tl, env, loc, cont))))
     
and apply_cont cont exp_val =
  match cont with
  | EndCont -> exp_val
  | DiffFstCont (exp2, env, cont) -> eval_exp exp2 env (DiffSndCont (exp_val, cont))
  | DiffSndCont (exp_val1, cont) ->
     (match (exp_val1, exp_val) with
      | (NumVal num1, NumVal num2) -> apply_cont cont (NumVal (num1-num2))
      | _ -> raise (ApplyContError ("DiffExp expects two integers")))
  | IsZeroCont cont ->
     (match exp_val with
      | NumVal num ->
         if num=0 then apply_cont cont (BoolVal true) else apply_cont cont (BoolVal false)
      | _ -> raise (ApplyContError ("IsZeroExp expects an integer")))
  | IfCont (exp1, exp2, env, cont) ->
     (match exp_val with
      | BoolVal b -> if b then eval_exp exp1 env cont else eval_exp exp2 env cont
      | _ -> raise (ApplyContError ("IfExp expects a boolean")))
  | LetCont (str, body, env, cont) ->
     eval_exp body (extend_env str exp_val env) cont
  | ApplyFstCont (exp, env, cont) ->
     (match exp with
      | VarExp (var, loc) ->
         (match exp_val with
          | ProcVal (str, body, p_env) ->
             eval_exp body ((str, (retrieve_ref_val var env)) :: !p_env) cont
          | _ -> raise (ApplyContError ("Apply is expecting a procedure.")))
      | _ -> eval_exp exp env (ApplySndCont (exp_val, cont)))
  | ApplySndCont (proc_val, cont) ->
     (match proc_val with
      | ProcVal (str, body, p_env) ->
         eval_exp body (extend_env str exp_val !p_env) cont
      | _ -> raise (ApplyContError ("Apply is expecting a procedure")))
  | MultiLetFstCont (str, env, tl, body, loc, cont) ->
     eval_exp (MultiLetExp (tl, body, loc)) (extend_env str exp_val env) cont
  | MultiLetSndCont (str, env, body, cont) ->
     eval_exp body (extend_env str exp_val env) cont
  | SetCont (str, env, cont) ->
     setref (retrieve_ref_val str env) exp_val;
     apply_cont cont (NumVal 1024)
  | BeginFstCont (tl, env, loc, cont) -> eval_exp (BeginExp (tl, loc)) env cont
  | BeginSndCont cont -> apply_cont cont exp_val
     
    
let eval_top_level (ExpTop e) =
  clear_store (); eval_exp e (empty_env ()) EndCont |> string_of_expval |> print_endline
                                                     
let value_of_program (AProgram tl_list) = List.iter eval_top_level tl_list 
