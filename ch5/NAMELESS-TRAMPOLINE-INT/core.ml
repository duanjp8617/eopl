open Syntax

type environment = expval list

and expval =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of expression * (environment ref)

type continuation =
  | EndCont
  | DiffFstCont of expression * environment * continuation
  | DiffSndCont of expval * continuation 
  | IsZeroCont of continuation 
  | IfCont of expression * expression * environment * continuation 
  | LetCont of expression * environment * continuation 
  | ApplyFstCont of expression * environment * continuation
  | ApplySndCont of expval * continuation 
  | MultiLetFstCont of (expval list) * environment * (expression list) * expression * continuation
                  
let string_of_expval value =
  match value with
  | NumVal n -> string_of_int n
  | BoolVal b -> string_of_bool b
  | ProcVal _ -> "proc"               
               
let empty_env () = []

let extend_env value env = value :: env
                                  
exception MissInEnv of Ploc.t
                  
let apply_env pos env =
  let rec go pos env =
    match env with
    | hd :: tl -> if pos = 0 then Some hd else go (pos-1) tl
    | [] -> None
  in
  (if pos = -1 then None else go pos env)
    
exception InterpreterError of string * Ploc.t
exception ApplyContError of string                          

type bounce =
  | ExpVal of expval
  | Bounce of (unit -> bounce)

let list_empty ls =
  match ls with
  | [] -> true
  | _ -> false
                          
let rec eval_exp exp env cont =
  match exp with
  | ConstExp (num, loc) -> apply_cont cont (NumVal num)
  | VarExp (pos, loc) ->
     (match (apply_env pos env) with
      | None -> raise (MissInEnv loc)
      | Some exp_val -> apply_cont cont exp_val)
  | ProcExp (body, loc) -> apply_cont cont (ProcVal (body, (ref env)))
  | DiffExp (exp1, exp2, loc) -> eval_exp exp1 env (DiffFstCont (exp2, env, cont))
  | IsZeroExp (exp, loc) -> eval_exp exp env (IsZeroCont cont)
  | IfExp(exp1, exp2, exp3, loc) -> eval_exp exp1 env (IfCont (exp2, exp3, env, cont))
  | LetExp (exp1, exp2, loc) -> eval_exp exp1 env (LetCont (exp2, env, cont))
  | ApplyExp(exp1, exp2, loc) -> eval_exp exp1 env (ApplyFstCont (exp2, env, cont))
  | LetRecExp(ls, body, loc) ->
     (let env_ref = ref [] in
      let proc_list = List.map (fun exp -> ProcVal (exp, env_ref)) ls in
      let new_env = List.append proc_list env in
      env_ref := new_env;
      eval_exp body new_env cont)
  | MultiLetExp (ls, body, loc) ->
     (match ls with
      | exp :: tl ->
         eval_exp exp env (MultiLetFstCont ([], env, tl, body, cont))
      | _ -> raise (InterpreterError ("MultiLetExp expects non-empty expression list", loc)))
     
                          
and apply_cont cont exp_val =
  match cont with
  | EndCont -> (ExpVal exp_val)
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
  | LetCont (body, env, cont) ->
     eval_exp body (exp_val :: env) cont
  | ApplyFstCont (exp, env, cont) ->
     eval_exp exp env (ApplySndCont (exp_val, cont))
  | ApplySndCont (proc_val, cont) ->
     (match proc_val with
      | ProcVal (body, p_env) ->
        Bounce (fun () -> eval_exp body (exp_val :: !p_env) cont)
      | _ -> raise (ApplyContError ("ApplyExp expects a procedure"))) 
  | MultiLetFstCont (exp_val_ls, env, tl, body, cont) ->
     (match tl with
      | exp :: let_def_tl ->
         eval_exp exp env
           (MultiLetFstCont (exp_val_ls @ [exp_val], env, let_def_tl, body, cont))
      | _ -> eval_exp body ((exp_val_ls @ [exp_val]) @ env) cont)

let rec tranpoline bounce_val =
  match bounce_val with
  | ExpVal value -> value
  | Bounce func -> tranpoline (func ())
    
let eval_top_level (ExpTop e) =
  tranpoline (eval_exp e (empty_env ()) EndCont) |> string_of_expval |> print_endline
                                                     
let value_of_program (AProgram tl_list) = List.iter eval_top_level tl_list 
